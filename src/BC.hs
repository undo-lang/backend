{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
module BC
    ( gen
    , Module
    , BCError
    ) where

--import Control.Monad.State (StateT(..), runStateT)
import GHC.Generics
import Debug.Trace
import Control.Lens ((^.))
import Control.Lens.Combinators (toListOf, over,  _1, _2, _3)
import Control.Lens.Fold (folded, (^..))
import Control.Lens.Plated (cosmos, children, universe, plate)
import Control.Lens.Wrapped
import Data.Aeson
import Data.List (nub, mapAccumL, elemIndex)
import qualified Data.Map as Map

import AST
import Scope (extractParams)

data BCError
 = DuplicateFunctionName String
 | DuplicateImport ModuleName
 | DuplicateFunctioNames
 deriving (Show)

data Instruction
 = PushInt Int
 | PushString Int
 | Call Int
 | JumpIfFalse Int -- how many to jump
 | Jump Int
 | LoadLocal Int
 | LoadGlobal String
 | LoadName ModuleName String
  deriving (Show, Generic, ToJSON)

type BcFn = (String, ParameterList, Block 'R)

getImports :: [Decl 'R] -> [ModuleName]
getImports = toListOf $ traverse . cosmos . _Import

getFunctions :: [Decl 'R] -> [BcFn]
getFunctions decls = decls^..folded._Fn

type StringTable = [String]

compileFns :: StringTable -> [BcFn] -> Map.Map String [Instruction]
compileFns strings fns = Map.fromList $ compileFn strings fnNames <$> fns
  where fnNames :: [String]
        fnNames = (\(n, _, _) -> n) <$> fns

-- TODO we need to pass the name of "globals" here
compileFn :: StringTable -> [String] -> BcFn -> (String, [Instruction])
compileFn strings fnNames (s, params, blk) = (s, compileBlock (extractParams params) blk)
  where compileBlock :: [String] -> Block 'R -> [Instruction]
        compileBlock prevLocals (Block lines) =
          -- Locals are append-only. Much like a "stack of variables", if we leave a function with 5 globals, we keep them there until later. This can be dangerous though: { {var a; a = 1} {var b; print b;}} = ??
          -- TODO generate `Store LocalIdx, null` for all the new variables, range (#prevLocals..#prevLocals + #locals]
          --      or find a better way (also make sure we dont for args)
          let locals = prevLocals ++ (lines^..folded._LineDecl._Var)
           in concat $ compileExpr locals <$> lines^..folded._LineExpr

        compileExpr :: [String] -> Expr 'R -> [Instruction]
        compileExpr locals (LitStr s) = case (s `elemIndex` strings) of
          Just idx -> [PushString idx]
          Nothing -> error ("ICE: String not found in table: " ++ s)
        compileExpr locals (LitNum n) =
          [PushInt n]
        compileExpr locals (CallExpr f xs) =
          (concat $ compileExpr locals <$> xs) ++ (compileExpr locals f) ++ [Call $ length xs]
        -- COND: DoCond; JumpIfFalse $END; DoBody; Jmp $COND; END: [...]
        compileExpr locals (LoopExpr cond blk) =
          let condInstrs = compileExpr locals cond
              bodyInstrs = compileBlock locals blk
              condBody = condInstrs++[JumpIfFalse $ 1 + length condInstrs + length bodyInstrs] -- 1 for the "jump back to cond"
          in condBody++bodyInstrs++[Jump $ negate $ length condBody + length bodyInstrs]
        -- DoCond; JumpIfFalse $ELSE; DoThen; Jmp $OUT; ELSE: DoElse; OUT: [...]
        compileExpr locals (ConditionalExpr cond then_ else_) =
          let condInstrs = compileExpr locals cond
              thenInstrs = (compileBlock locals then_)++[JumpIfFalse $ length elseInstrs]
              elseInstrs = compileBlock locals else_
              jmpToElse = [JumpIfFalse $ length thenInstrs]
          in condInstrs++jmpToElse++thenInstrs++elseInstrs
        compileExpr locals (NameExpr (Local s)) = case ((s `elemIndex` locals), (s `elemIndex` fnNames)) of
          (Just idx, _) -> [LoadLocal idx]
          (Nothing, Just _) -> [LoadGlobal s]
          (Nothing, Nothing) -> error ("ICE: No such local: '" ++ s ++ "', locals = " ++ show locals ++ ". Btw, globals vars are NYI, only global fns.")
        compileExpr _ (NameExpr (Namespaced ns name)) =
          [LoadName ns name]

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
  , functions :: Map.Map String [Instruction]
  , strings :: StringTable
  }
  deriving (Show, Generic, ToJSON)
