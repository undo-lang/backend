{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module BC
    ( gen
    , Module
    , BCError
    ) where

--import Control.Monad.State (StateT(..), runStateT)
import GHC.Generics
import Debug.Trace
import Control.Lens ((^.), Lens', lens, (<<+~), (<>~), (<<+=), (<>=), at, (?~), (%=), uses)
import Control.Lens.Combinators (toListOf, over,  _1, _2, _3)
import Control.Lens.Fold (folded, (^..))
import Control.Lens.Plated (cosmos, children, universe, plate)
import Control.Lens.Wrapped
import Data.Foldable (traverse_)
import Data.Aeson
import Control.Monad.State
import Data.List (nub, mapAccumL, elemIndex)
import qualified Data.Map as Map

import AST
import Scope (extractParams)

data BCError
 = DuplicateFunctionName String
 | DuplicateImport ModuleName
 | DuplicateFunctioNames
 deriving (Show)

newtype LabelIdx = LabelIdx Int
  deriving (Show, Generic, Eq, Ord)
deriving instance ToJSON LabelIdx
data JumpStage = L | O -- Label / Offset
data JumpData :: JumpStage -> * where
  Label :: LabelIdx -> JumpData 'L
  Offset :: Int -> JumpData 'O
deriving instance Show (JumpData s)
instance ToJSON (JumpData s) where
  toJSON (Offset x) = object [ "offset" .= x ]

data Instruction s
 = PushInt Int
 | PushString Int
 | Call Int
 | Unless (JumpData s)
 | Jump (JumpData s)
 | LoadLocal Int
 | LoadGlobal String
 | LoadName ModuleName String
  deriving (Show, Generic)
deriving instance ToJSON (Instruction s) -- ???

type BcFn = (String, ParameterList, Block 'R)

getImports :: [Decl 'R] -> [ModuleName]
getImports = toListOf $ traverse . cosmos . _Import

getFunctions :: [Decl 'R] -> [BcFn]
getFunctions decls = decls^..folded._Fn

type StringTable = [String] 

type LabelMap = Map.Map LabelIdx Int
data Builder = Builder
  { lastLabel :: Int
  , instrs :: [Instruction 'L]
  , resolvedLabels :: LabelMap
  }
emptyBuilder = Builder { lastLabel = 0, instrs = [] }

_lastLabel :: Lens' Builder Int
_lastLabel = lens lastLabel (\s a -> s { lastLabel = a })

_instrs :: Lens' Builder [Instruction 'L]
_instrs = lens instrs (\s a -> s { instrs = a })

_resolvedLabels :: Lens' Builder LabelMap
_resolvedLabels = lens resolvedLabels (\s a -> s { resolvedLabels = a })

generateLabel :: MonadState Builder m => m Int
generateLabel = _lastLabel <<+= 1

appendInstr :: MonadState Builder m => Instruction 'L -> m ()
appendInstr = (_instrs <>=) . pure

--resolveLabel :: MonadState Builder m => LabelIdx -> m ()
resolveLabel labelIdx = do i <- uses _instrs length
                           _resolvedLabels %= (at labelIdx ?~ i)
--resolveLabel labelIdx = do instrs <- _instrs
--                           at labelIdx ?~ length (instrs.instrs)

makeLabel = Label . LabelIdx

type BuilderState = State Builder ()

compileFn'' :: StringTable -> BcFn -> Builder
compileFn'' strings (s, params, blk) = execState (compileBlock blk) emptyBuilder
  where compileBlock :: Block 'R -> BuilderState
        compileBlock (Block lines) = traverse_ compileExpr $ lines^..folded._LineExpr

        compileExpr :: Expr 'R -> BuilderState
        compileExpr (LitStr s) = case (s `elemIndex`) strings of
          Just idx -> appendInstr $ PushString idx
          Nothing -> error $ "ICE: String not found in table: " ++ s
        compileExpr (LitNum n) =
          appendInstr $ PushInt n
        compileExpr (CallExpr f xs) = do
          traverse_ compileExpr xs
          compileExpr f
          appendInstr $ Call $ length xs
        compileExpr (ConditionalExpr cond then_ else_) = do
          endLabel <- generateLabel
          elseLabel <- generateLabel
          compileExpr cond
          appendInstr $ Unless $ makeLabel elseLabel
          compileBlock then_
          appendInstr $ Jump $ makeLabel endLabel
          resolveLabel $ LabelIdx $ elseLabel -- TODO not this whole LabelIdx dance
          compileBlock else_
          resolveLabel $ LabelIdx $ endLabel
        compileExpr (LoopExpr cond blk) = do
          -- TODO break etc, endLabel + some kind of stack of "break; points"
          startLabel <- generateLabel
          endLabel <- generateLabel
          resolveLabel $ LabelIdx startLabel
          compileExpr cond
          appendInstr $ Unless $ makeLabel endLabel
          compileBlock blk
          appendInstr $ Jump $ makeLabel startLabel
          resolveLabel $ LabelIdx endLabel
        compileExpr (NameExpr (Local name)) = error "NYI"
        compileExpr (NameExpr (Namespaced ns name)) = error "NYI"

        compileExpr _ = do labelIdx <- generateLabel
                           let label = Label $ LabelIdx labelIdx
                           appendInstr $ Jump label

        --compileLine :: Line 'R -> BuilderState
        --compileLine line = do labelIdx <- generateLabel
        --                      let label = Label $ LabelIdx labelIdx
        --                      appendInstr $ Jump label

-- compileFn' :: StringTable -> BcFn -> Builder
-- compileFn' strings (s, params, blk) = compileBlock emptyBuilder blk
--   where compileBlock :: Builder -> Block 'R -> Builder 
--         compileBlock builder (Block lines) = foldl compileLine builder lines
-- 
--         compileLine :: Builder -> Line 'R -> Builder
--         compileLine s _ = let (idx, newS) =  generateLabel s
--                           in appendInstr (Jump $ Label $ LabelIdx idx) newS

compileFns :: StringTable -> [BcFn] -> Map.Map String [Instruction 'O]
compileFns strings fns = Map.fromList []
  where fnNames :: [String]
        fnNames = (\(n, _, _) -> n) <$> fns

-- Moves Leftover top-level code to a function called Main
reformatBlock :: Block 'R -> [Decl 'R]
reformatBlock (Block lines) = (genMain exprs):decls
  where exprs :: [Expr 'R]
        exprs = lines^..folded._LineExpr

        decls :: [Decl 'R]
        decls = lines^..folded._LineDecl

        genMain :: [Expr 'R] -> Decl 'R
        genMain exprs = Fn "MAIN" (ParameterList []) (Block $ LineExpr <$> exprs)

-- Returns all strings. Right now, doesn't keep Namespaced names (need to mangle them)
getStrs :: BcFn -> [String]
getStrs (s, _, (Block lines)) = [s] ++ litStrs exprs ++ names exprs
  where exprs :: [Expr 'R]
        exprs = lines^..folded._LineExpr

        litStrs = toListOf $ traverse . cosmos . _LitStr

        names = toListOf $ traverse . cosmos . _NameExpr . _Local

gen :: ModuleName -> Block 'R -> Either BCError Module
gen name block = do
  let decls = reformatBlock block
  let fns = getFunctions decls
  let imports = getImports decls
  let strings = nub $ concat (getStrs <$> fns)
  let functions = compileFns strings fns
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
