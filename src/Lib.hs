{-# LANGUAGE DataKinds #-}
module Lib
  ( compile
  ) where

import qualified Data.ByteString.Lazy as BS

import Control.Lens (_Left, _Right, set, over)
import Data.Aeson (eitherDecode)

import AST
import Scope (resolveRoot, ScopeError)

data Error
  = ParseError String
  | ScopingError ScopeError
  deriving (Show)

compile :: BS.ByteString -> IO ()
compile contents = putStrLn $ show $ do
  block <- readBlock contents
  resolved <- readRoot block
  let decls = reformatBlock resolved
  pure $ decls

readBlock :: BS.ByteString -> Either Error (Block 'U)
readBlock = over _Left ParseError . eitherDecode

readRoot :: Block 'U -> Either Error (Block 'R)
readRoot = over _Left ScopingError . resolveRoot


-- Moves Leftover top-level code to a function called Main
reformatBlock :: Block 'R -> [Decl 'R]
reformatBlock (Block lines) = let (decls, exprs) = go lines [] []
                               in (genMain exprs):reverse decls
  where go :: [Line 'R] -> [Decl 'R] -> [Expr 'R] -> ([Decl 'R], [Expr 'R])
        go ((LineDecl decl):lines) decls exprs = go lines (decl:decls) exprs
        go ((LineExpr expr):lines) decls exprs = go lines decls (expr:exprs)

        genMain :: [Expr 'R] -> Decl 'R
        genMain exprs = Fn "MAIN" (ParameterList []) (Block $ LineExpr <$> exprs)
