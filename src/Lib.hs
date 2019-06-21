{-# LANGUAGE DataKinds #-}
module Lib
  ( compile
  ) where

import qualified Data.ByteString.Lazy as BS

import Debug.Trace
import Control.Lens (_Left, _Right, set, over)
import Data.Aeson (eitherDecode)

import AST
import Scope (resolveRoot, ScopeError)
import BC (gen, Module, BCError)

data Error
  = ParseError String
  | ScopingError ScopeError
  | BytecodeError BCError
  deriving (Show)

compile :: String -> BS.ByteString -> IO ()
compile moduleName contents = putStrLn $ show $ do
  block <- traceShowId $ readBlock contents
  resolved <- traceShowId $ readRoot block
  bc <- genBC moduleName resolved
  pure $ bc

readBlock :: BS.ByteString -> Either Error (Block 'U)
readBlock = over _Left ParseError . eitherDecode

readRoot :: Block 'U -> Either Error (Block 'R)
readRoot = over _Left ScopingError . resolveRoot

genBC :: String -> Block 'R -> Either Error Module
genBC moduleName block = over _Left BytecodeError $ gen moduleName block
