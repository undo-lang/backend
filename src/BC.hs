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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
--{-# LANGUAGE DisambiguateRecordFields #-}
--{-# LANGUAGE NoFieldSelectors #-}

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
import Data.Foldable (traverse_, for_)
import Control.Monad (replicateM)
import Control.Monad.Except (ExceptT, runExceptT, MonadError (throwError))
--import Data.Functor (($>))
import Data.Aeson
import Control.Monad.State
import Data.List (elemIndex)
import Data.Kind (Type)
import qualified Data.Map as Map

import AST
import Scope (extractParams)

data InternalCompilerError
    = LocalNotResolved String [String] -- (missing variable, list of available variables)
    | MatchLocalNotResolved String [String] -- (missing variable, list of available variables)
    deriving (Show)

data BCError
 = DuplicateFunctionName String
 | DuplicateImport ModuleName
 | DuplicateFunctioNames
 | LabelNotResolved LabelIdx
 | UnresolvedModule ModuleName
 | ICE InternalCompilerError
 deriving (Show)

newtype LabelIdx = LabelIdx Int
  deriving (Show, Generic, Eq, Ord)
newtype RegisterIdx = RegisterIdx Int
  deriving (Show, Generic, Eq, Ord, ToJSON, FromJSON)
deriving instance ToJSON LabelIdx
data BCStage = L | O -- Label / Offset
data JumpData :: BCStage -> Type where
  Label :: LabelIdx -> JumpData 'L
  Offset :: Int -> JumpData 'O
deriving instance Show (JumpData s)
instance ToJSON (JumpData 'O) where
  toJSON (Offset x) = toJSON x

data BCModuleName :: BCStage -> Type where
  UnresolvedModuleName :: ModuleName -> BCModuleName 'L
  CurrentModuleName :: BCModuleName 'L
  ResolvedModuleName :: ModuleName -> BCModuleName 'O
deriving instance Show (BCModuleName s)
instance ToJSON (BCModuleName 'O) where
  toJSON (ResolvedModuleName (ModuleName m)) = object [ "module" .= m ]

data Instruction s
 = PushInt Int
 | PushString String
 | Call Int
 | JumpUnless (JumpData s)
 | Jump (JumpData s)
 | LoadLocal Int
 | StoreLocal Int
 | LoadGlobal String
 | LoadRegister RegisterIdx
 | StoreRegister RegisterIdx
 | LoadName (BCModuleName s) String
 | Instantiate (BCModuleName s) String String -- Module ADT Ctor
 | IsVariant (BCModuleName s) String String -- Module ADT Ctor
 | Field (BCModuleName s) String String String -- Module ADT Ctor Field
  deriving (Show, Generic)
deriving instance ToJSON (Instruction 'O) -- ???

type BcFn = (String, ParameterList, Block 'R)

data ADTVariant = ADTVariant
  { name :: String
  , elements :: [String] }
  deriving (Show, Generic, ToJSON)

data ExpectedADT = ExpectedADT
  { module_ :: ModuleName
  , adtName :: String
  , variants :: [ADTVariant] }
  deriving (Show)

instance ToJSON ExpectedADT where
  toJSON ExpectedADT {module_, adtName, variants} =
    object ["module" .= module_, "name" .= adtName, "variants" .= variants]

data Module = Module -- TODO version?
  { name :: ModuleName
  , dependencies :: [ModuleName] -- this should include all `Import`s (dupe imports = error)
  , functions :: Map.Map String [Instruction 'O]
  , adts :: Map.Map String [ADTVariant]
  , expectedAdts :: [ExpectedADT]
  }
  deriving (Show, Generic, ToJSON)


getImports :: [Decl 'R] -> [ModuleName]
getImports = toListOf $ traverse . cosmos . _Import

getFunctions :: [Decl 'R] -> [BcFn]
getFunctions decls = decls^..folded._Fn

-- Globally namespaced enums (XXX should check that there are no overlaps)
getEnums :: [Decl 'R] -> Map.Map String [ADTVariant]
getEnums = fmap (fmap remap) . Map.fromList . toListOf (traverse . cosmos . _Enum)
  where
  -- map to a new type that's serializable
    remap (EnumVariant v es) = ADTVariant v es

type LabelMap = Map.Map LabelIdx Int
data Builder = Builder
  { _lastLabel :: Int
  , _lastRegister :: Int
  , _instrs :: [Instruction 'L]
  , _resolvedLabels :: LabelMap
  }

$(makeLenses ''Builder)

emptyBuilder :: Builder
emptyBuilder = Builder 0 0 [] Map.empty

generateLabel :: MonadState Builder m => m LabelIdx
generateLabel = LabelIdx <$> (lastLabel <<+= 1)

appendInstr :: MonadState Builder m => Instruction 'L -> m ()
appendInstr = (instrs <>=) . pure

-- TODO fail if already resolved
resolveLabel :: MonadState Builder m => LabelIdx -> m ()
resolveLabel labelIdx = do i <- uses instrs length
                           resolvedLabels %= (at labelIdx ?~ i)

allocateRegister :: MonadState Builder m => m RegisterIdx
allocateRegister = RegisterIdx <$> (lastRegister <<+= 1)

registerSave :: MonadState Builder m => m RegisterIdx
registerSave = do
  register <- allocateRegister
  appendInstr $ StoreRegister register
  pure register

jump :: LabelIdx -> Instruction 'L
jump = Jump . Label
jumpUnless :: LabelIdx -> Instruction 'L
jumpUnless = JumpUnless . Label

type BuilderState = ExceptT BCError (State Builder)

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

addBreakAndContinueTarget  :: LabelIdx -> Scope -> Scope
addBreakAndContinueTarget label scope = label `addBreakTarget` (label `addContinueTarget` scope)

emptyScope :: Scope
emptyScope = ([], [], [])

resolveBuilder :: ModuleName -> Builder -> Either BCError [Instruction 'O]
resolveBuilder moduleName builder = traverse resolve $ builder^.instrs
  where resolve :: Instruction 'L -> Either BCError (Instruction 'O)
        resolve (Jump loc) = Jump <$> resolveLoc loc
        resolve (JumpUnless loc) = JumpUnless <$> resolveLoc loc
        resolve (LoadName m v) = LoadName <$> resolveModuleName m <*> pure v
        resolve (PushInt x) = Right $ PushInt x
        resolve (PushString x) = Right $ PushString x
        resolve (Call x) = Right $ Call x
        resolve (LoadLocal x) = Right $ LoadLocal x
        resolve (StoreLocal x) = Right $ StoreLocal x
        resolve (LoadGlobal x) = Right $ LoadGlobal x
        resolve (LoadRegister r) = Right $ LoadRegister r
        resolve (StoreRegister r) = Right $ StoreRegister r
        resolve (Instantiate ns e c) = Instantiate <$> resolveModuleName ns <*> pure e <*> pure c
        resolve (IsVariant ns e c) = IsVariant <$> resolveModuleName ns <*> pure e <*> pure c
        resolve (Field ns e c f) = Field <$> resolveModuleName ns <*> pure e <*> pure c <*> pure f

        resolveModuleName :: BCModuleName 'L -> Either BCError (BCModuleName 'O)
        resolveModuleName CurrentModuleName = Right $ ResolvedModuleName moduleName
        resolveModuleName (UnresolvedModuleName (ModuleName [])) = Right $ ResolvedModuleName moduleName
        resolveModuleName (UnresolvedModuleName mod) = if True -- TODO if mod `elem` imports
          then Right $ ResolvedModuleName mod
          else Left $ UnresolvedModule mod

        resolveLoc :: JumpData 'L -> Either BCError (JumpData 'O)
        resolveLoc (Label idx) = case Map.lookup idx (builder^.resolvedLabels) of
            Just o -> Right $ Offset o
            Nothing -> Left $ LabelNotResolved idx

compileFn :: ModuleName -> [String] -> BcFn -> Either BCError [Instruction 'O]
compileFn moduleName fnNames (_, params, blk) =
  let scope = extractParams params `addLocals` emptyScope
  in case runState (runExceptT (compileBlock scope blk)) emptyBuilder of
                    (Right (), builder) -> resolveBuilder moduleName builder
                    (Left err, _) -> Left err
  where compileBlock :: Scope -> Block 'R -> BuilderState ()
        compileBlock scope (Block lines) =
          let newScope = getDecls lines `addLocals` scope
          in traverse_ (compile newScope) $ lines^..folded

        compile :: Scope -> Line 'R -> BuilderState ()
        compile s (LineDecl decl) = compileDecl s decl
        compile s (LineExpr expr) = compileExpr s expr

        compileDecl :: Scope -> Decl 'R -> BuilderState ()
        compileDecl scope (Var name mbInit) = for_ mbInit $ \init ->
          case name `elemIndex` (scope^._locals) of
            Just idx -> do
              compileExpr scope init
              appendInstr $ StoreLocal idx
            Nothing -> throwError $ ICE $ LocalNotResolved name (scope^._locals)

        compileDecl _ _ = pure ()

        compileExpr :: Scope -> Expr 'R -> BuilderState ()
        compileExpr _ (LitStr s) =
          appendInstr $ PushString s
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
            Nothing -> throwError $ ICE $ LocalNotResolved name (scope^._locals)
        compileExpr _ (NameExpr (Namespaced ns name)) =
          appendInstr $ LoadName (UnresolvedModuleName ns) name
        compileExpr scope (MatchExpr topic bs) = do
          failLabel <- generateLabel
          endLabel <- generateLabel
          compileExpr scope topic
          reg <- registerSave
          -- Prepare labels for each case, so we can jump to the next when the match fails
          labels <- replicateM (length bs - 1) generateLabel
          let startLabels = Nothing:fmap Just labels
              endLabels = labels ++ [failLabel]
          traverse_ (compileMatchBranch scope endLabel reg) (zip3 bs startLabels endLabels)
          resolveLabel failLabel
          -- TODO error instr? prelude error fn?
          resolveLabel endLabel

        compileMatchBranch scope end reg (MatchBranch pattern b, startLabel, nextBranch) = do
          for_ startLabel resolveLabel -- Resolve our "Start label" unless we're the 1st branch
          let innerScope = matchNames pattern `addLocals` scope
          -- pass the new scope to the pattern so it knows where to bind
          compilePattern innerScope reg nextBranch pattern
          compileBlock innerScope b
          appendInstr $ jump end

        compilePattern :: Scope -> RegisterIdx -> LabelIdx -> MatchSubject 'R -> BuilderState ()
        compilePattern scope reg nextBranch = \case
          MatchSubjectConstructor (ResolvedVariant mn e c) vars -> do
            appendInstr $ LoadRegister reg
            appendInstr $ IsVariant (UnresolvedModuleName mn) e c
            appendInstr $ jumpUnless nextBranch
            for_ vars $ \(fieldName, var) -> do
              appendInstr $ LoadRegister reg
              appendInstr $ Field (UnresolvedModuleName mn) e c fieldName
              fieldReg <- registerSave
              compilePattern scope fieldReg nextBranch var
          MatchSubjectVariable name -> do
            appendInstr $ LoadRegister reg
            case name `elemIndex` (scope^._locals) of
              Just idx -> appendInstr $ StoreLocal idx
              Nothing -> throwError $ ICE $ MatchLocalNotResolved name (scope^._locals)

        -- TODO break, continue etc

        getDecls lines = fst <$> lines^..folded._LineDecl._Var

        --compileLine :: Line 'R -> BuilderState ()
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

gen :: ModuleName -> Block 'R -> Either BCError Module
gen name block = do
  let decls = reformatBlock block
      fns = getFunctions decls
      dependencies = getImports decls
      adts = getEnums decls
      expectedAdts = []

  functions <- compileFns name fns
  pure Module { name, dependencies, functions, adts, expectedAdts }
