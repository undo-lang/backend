{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module BC
    ( gen
    , Module
    , BCError
    ) where

--import Control.Monad.State (StateT(..), runStateT)
import GHC.Generics
--import Debug.Trace
import Control.Lens ((^.), Lens', (<>~), (<<+=), (<>=), at, (?~), (%=), uses)
import Control.Lens.Combinators (toListOf, _1, _2, _3)
import Control.Lens.TH (makeLenses)
import Control.Lens.Fold (folded, (^..))
import Control.Lens.Plated (cosmos) -- (children, universe, plate)
import Data.Foldable (traverse_)
--import Data.Functor (($>))
import Data.Aeson
import Control.Monad.State
import Data.List (elemIndex)
import Data.Kind (Type)
import qualified Data.Map as Map

import AST
import Scope (extractParams)

data BCError
 = DuplicateFunctionName String
 | DuplicateImport ModuleName
 | DuplicateFunctioNames
 | LabelNotResolved LabelIdx
 | UnresolvedModule ModuleName
 deriving (Show)

newtype LabelIdx = LabelIdx Int
  deriving (Show, Generic, Eq, Ord)
deriving instance ToJSON LabelIdx
data BCStage = L | O -- Label / Offset
data JumpData :: BCStage -> Type where
  Label :: LabelIdx -> JumpData 'L
  Offset :: Int -> JumpData 'O
deriving instance Show (JumpData s)
instance ToJSON (JumpData 'O) where
  -- TODO ToJSON (JumpData 'O)?
  toJSON (Offset x) = object [ "offset" .= x ]

data BCModuleName :: BCStage -> Type where
  UnresolvedModuleName :: ModuleName -> BCModuleName 'L
  CurrentModuleName :: BCModuleName 'L
  ResolvedModuleName :: ModuleName -> BCModuleName 'O
deriving instance Show (BCModuleName  s)
instance ToJSON (BCModuleName 'O) where
  toJSON (ResolvedModuleName (ModuleName m)) = object [ "module" .= m ]

data Instruction s
 = PushInt Int
 | PushString Int
 | Call Int
 | JumpUnless (JumpData s)
 | Jump (JumpData s)
 | LoadLocal Int
 | LoadGlobal String
 | LoadName (BCModuleName s) String
  deriving (Show, Generic)
deriving instance ToJSON (Instruction 'O) -- ???

type BcFn = (String, ParameterList, Block 'R)

getImports :: [Decl 'R] -> [ModuleName]
getImports = toListOf $ traverse . cosmos . _Import

getFunctions :: [Decl 'R] -> [BcFn]
getFunctions decls = decls^..folded._Fn

type StringTable = [String] 

type LabelMap = Map.Map LabelIdx Int
data Builder = Builder
  { _lastLabel :: Int
  , _instrs :: [Instruction 'L]
  , _resolvedLabels :: LabelMap
  }

$(makeLenses ''Builder)

emptyBuilder :: Builder
emptyBuilder = Builder 0 [] Map.empty

generateLabel :: MonadState Builder m => m LabelIdx
generateLabel = LabelIdx <$> (lastLabel <<+= 1)

appendInstr :: MonadState Builder m => Instruction 'L -> m ()
appendInstr = (instrs <>=) . pure

resolveLabel :: MonadState Builder m => LabelIdx -> m ()
resolveLabel labelIdx = do i <- uses instrs length
                           resolvedLabels %= (at labelIdx ?~ i)

jump :: LabelIdx -> Instruction 'L
jump = Jump . Label
jumpUnless :: LabelIdx -> Instruction 'L
jumpUnless = JumpUnless . Label

type BuilderState = State Builder ()

-- (break, continue, locals)
type Scope = ([LabelIdx], [LabelIdx], [String])
_breakTargets :: Lens' Scope [LabelIdx]
_breakTargets = _1

_continueTargets :: Lens' Scope [LabelIdx]
_continueTargets = _2

_locals :: Lens' Scope [String]
_locals = _3

addBreakTarget :: LabelIdx -> Scope -> Scope
addBreakTarget = (_breakTargets <>~) . pure

addContinueTarget  :: LabelIdx -> Scope -> Scope
addContinueTarget = (_breakTargets <>~) . pure

addLocals   :: [String] -> Scope -> Scope
addLocals = (_locals <>~)

addBreakAndContinueTarget   :: LabelIdx -> Scope -> Scope
addBreakAndContinueTarget label scope = label `addBreakTarget` (label `addContinueTarget` scope)

emptyScope :: Scope
emptyScope = ([], [], [])

resolveBuilder :: ModuleName -> Builder -> Either BCError [Instruction 'O]
resolveBuilder moduleName builder = traverse resolve $ builder^.instrs
  where resolve :: Instruction 'L -> Either BCError (Instruction 'O)
        resolve (Jump loc) = Jump <$> resolveLoc loc
        resolve (JumpUnless loc) = JumpUnless <$> resolveLoc loc
        resolve (PushInt x) = Right $ PushInt x
        resolve (PushString x) = Right $ PushString x
        resolve (Call x) = Right $ Call x
        resolve (LoadLocal x) = Right $ LoadLocal x
        resolve (LoadGlobal x) = Right $ LoadGlobal x
        resolve (LoadName CurrentModuleName v) = Right $ LoadName (ResolvedModuleName moduleName) v
        resolve (LoadName (UnresolvedModuleName mod) v) = if True -- TODO if mod `elem` imports
          then Right $ LoadName (ResolvedModuleName mod) v
          else Left $ UnresolvedModule mod

        resolveLoc :: JumpData 'L -> Either BCError (JumpData 'O)
        resolveLoc (Label idx) = case Map.lookup idx (builder^.resolvedLabels) of
            Just o -> Right $ Offset o
            Nothing -> Left $ LabelNotResolved idx

compileFn :: ModuleName -> [String] -> BcFn -> Either BCError [Instruction 'O]
compileFn moduleName fnNames (_, params, blk) =
  let scope = extractParams params `addLocals` emptyScope
  in resolveBuilder moduleName $ execState (compileBlock scope blk) emptyBuilder
  where compileBlock :: Scope -> Block 'R -> BuilderState
        compileBlock scope (Block lines) =
          let newScope = getDecls lines `addLocals` scope
          in traverse_ (compileExpr newScope) $ lines^..folded._LineExpr

        compileExpr :: Scope -> Expr 'R -> BuilderState
        compileExpr _ (InternedStr idx) = appendInstr $ PushString idx
        compileExpr _ (LitNum n) =
          appendInstr $ PushInt n
        compileExpr scope (CallExpr f xs) = do
          traverse_ (compileExpr scope) xs
          compileExpr scope f
          appendInstr $ Call $ length xs
        compileExpr scope (ConditionalExpr cond then_ else_) = do
          endLabel <- generateLabel
          elseLabel <- generateLabel
          compileExpr scope cond
          appendInstr $ jumpUnless elseLabel
          compileBlock scope then_
          appendInstr $ jump endLabel
          resolveLabel elseLabel
          compileBlock scope else_
          resolveLabel endLabel
        compileExpr scope (LoopExpr cond blk) = do
          startLabel <- generateLabel
          endLabel <- generateLabel
          resolveLabel startLabel
          compileExpr scope cond
          appendInstr $ jumpUnless endLabel
          compileBlock (endLabel `addBreakAndContinueTarget` scope) blk
          appendInstr $ jump startLabel
          resolveLabel endLabel
        compileExpr scope (NameExpr (Local name)) = case name `elemIndex` (scope^._locals) of
          Just idx -> appendInstr $ LoadLocal idx
          Nothing -> case name `elemIndex` fnNames of
            Just _ -> appendInstr $ LoadGlobal name --LoadName CurrentModuleName name
            Nothing -> error $ "local not found. want: " ++ name ++ ", got = " ++ show (scope^._locals)
        compileExpr _ (NameExpr (Namespaced ns name)) =
          appendInstr $ LoadName (UnresolvedModuleName ns) name
        compileExpr scope (MatchExpr topic bs) = do
          endLabel <- generateLabel
          compileExpr scope topic -- TODO local or something...
          traverse_ (compileMatchBranch scope endLabel) bs
          resolveLabel endLabel

        compileMatchBranch scope end (MatchBranch _s b) = do
          -- TODO generate the `if`, jump to `noMatch` if it fails
          noMatch <- generateLabel
          compileBlock scope b
          appendInstr $ jump end
          resolveLabel noMatch


        -- TODO break, continue etc

        getDecls lines = lines^..folded._LineDecl._Var

        --compileLine :: Line 'R -> BuilderState
        --compileLine line = do labelIdx <- generateLabel
        --                      let label = Label $ LabelIdx labelIdx
        --                      appendInstr $ Jump label

compileFns :: ModuleName -> [BcFn] -> Either BCError (Map.Map String [Instruction 'O])
compileFns moduleName fns = Map.fromList <$> traverse compile fns
  where name (n, _, _) = n
        fnNames = name <$> fns
        compile o = (name o,) <$> compileFn moduleName fnNames o

-- Moves Leftover top-level code to a function called Main
reformatBlock :: Block 'R -> [Decl 'R]
reformatBlock (Block lines) = genMain exprs:decls
  where exprs :: [Expr 'R]
        exprs = lines^..folded._LineExpr

        decls :: [Decl 'R]
        decls = lines^..folded._LineDecl

        genMain :: [Expr 'R] -> Decl 'R
        genMain exprs = Fn "MAIN" (ParameterList []) (Block $ LineExpr <$> exprs)

gen :: ModuleName -> [String] -> Block 'R -> Either BCError Module
gen name strings block = do
  let decls = reformatBlock block
  let fns = getFunctions decls
  let imports = getImports decls
  functions <- compileFns name fns
  pure Module
       { name = name
       , dependencies = imports
       , functions = functions
       , strings = strings }

data Module = Module -- TODO version?
  { name :: ModuleName
  , dependencies :: [ModuleName] -- this should include all `Import`s (dupe imports = error)
  , functions :: Map.Map String [Instruction 'O]
  , strings :: StringTable
  }
  deriving (Show, Generic, ToJSON)
