{-# LANGUAGE DataKinds #-}
module BC where

--import Control.Monad.State (StateT(..), runStateT)
import Debug.Trace
import Control.Lens.Combinators (toListOf, over, _1, _2, _3)
import Control.Lens.Fold (folded, (^..))
import Control.Lens.Plated (cosmos, children, universe, plate)
import Control.Lens.Wrapped
import Data.List (nub, mapAccumL)
import qualified Data.Map as Map

import AST

data BCError
 = DuplicateFunctionName String
 | DuplicateImport ModuleName
 | DuplicateFunctioNames
 deriving (Show)

data Instruction
 = PushInt Int
 | PushString String
 | StaticCall String Int
  deriving (Show)

type BcFn = (String, ParameterList, Block 'R)

getImports :: [Decl 'R] -> [ModuleName]
getImports = toListOf $ traverse . cosmos . _Import

getFunctions :: [Decl 'R] -> Either BCError [BcFn]
getFunctions decls = Right $ decls^..folded._Fn

type StringTable = [String]

compileFns :: [BcFn] -> Map.Map String [Instruction]
compileFns fns = Map.fromList $ compileFn <$> fns

-- TODO we need to pass the name of "globals" here
compileFn :: BcFn -> (String, [Instruction])
compileFn (s, _param, _block) = (s, []) -- TODO

-- Moves Leftover top-level code to a function called Main
reformatBlock :: Block 'R -> [Decl 'R]
reformatBlock (Block lines) = (genMain exprs):decls
  where exprs :: [Expr 'R]
        exprs = lines^..folded._LineExpr

        decls :: [Decl 'R]
        decls = lines^..folded._LineDecl

        genMain :: [Expr 'R] -> Decl 'R
        genMain exprs = Fn "MAIN" (ParameterList []) (Block $ LineExpr <$> exprs)

-- get all strings... except function names
getStrs :: BcFn -> [String]
getStrs (s, _, (Block lines)) = [s] ++ litStrs exprs ++ names exprs
  where exprs :: [Expr 'R]
        exprs = lines^..folded._LineExpr

        litStrs = toListOf $ traverse . cosmos . _LitStr

        names = toListOf $ traverse . cosmos . _NameExpr . _Local

gen :: String -> Block 'R -> Either BCError Module
gen name block = do
  let decls = reformatBlock block
  fns <- getFunctions decls
  let imports = getImports decls
  let strings = nub $ concat (getStrs <$> fns)
  let functions = compileFns fns
  pure Module
       { name = name
       , dependencies = imports
       , functions = functions
       , strings = strings }

data Module = Module -- TODO version?
  { name :: String
  , dependencies :: [ModuleName] -- this should include all `Import`s (dupe imports = error)
  , functions :: Map.Map String [Instruction]
  , strings :: StringTable
  }
  deriving (Show)
