{-# LANGUAGE DataKinds #-}
module BC where

import Control.Monad (ap)
import Control.Lens.Combinators (toListOf, _1)
import Control.Lens.Fold (folded, (^..))
import Control.Lens.Plated (cosmos)
import Data.List (nub)
import qualified Data.Map as Map

import AST

data BCError
 = DuplicateFunctionName String
 | DuplicateImport ModuleName
 deriving (Show)

data Instruction
 = PushInt Int
 | PushString String
 | StaticCall String Int
  deriving (Show)

getImports :: [Decl 'R] -> [ModuleName]
getImports = toListOf $ traverse . cosmos . _Import

getFunctions :: [Decl 'R] -> Either BCError [(String, ParameterList, Block 'R)]
getFunctions decls = if ap (==) nub fnNames then Right fns else Left undefined
  where fns :: [(String, ParameterList, Block 'R)]
        fns = decls^..folded._Fn

        fnNames :: [String]
        fnNames = fns^..folded._1

-- TODO we need to pass the name of "globals" here
compileFn :: (String, ParameterList, Block s) -> (String, [Instruction])
compileFn (s, param, block) = (s, []) -- TODO

-- Moves Leftover top-level code to a function called Main
reformatBlock :: Block 'R -> [Decl 'R]
reformatBlock (Block lines) = (genMain exprs):decls
  where exprs :: [Expr 'R]
        exprs = lines^..folded._LineExpr

        decls :: [Decl 'R]
        decls = lines^..folded._LineDecl

        genMain :: [Expr 'R] -> Decl 'R
        genMain exprs = Fn "MAIN" (ParameterList []) (Block $ LineExpr <$> exprs)

gen :: String -> Block 'R -> Either BCError Module -- TODO return Either Error
gen name block = do
  let decls = reformatBlock block
  fns <- getFunctions decls
  let functions = compileFn <$> fns
  pure Module
       { name = name
       , dependencies = getImports decls
       , functions = Map.fromList functions }

-- TODO global strings (function names etc), probably using Plated+State

data Module = Module -- TODO version?
  { name :: String
  , dependencies :: [ModuleName] -- this should include all `Import`s (dupe imports = error)
  , functions :: Map.Map String [Instruction]
  }
  deriving (Show)
