{-# LANGUAGE DataKinds #-}
module Lib
  ( compile
  , Error
  ) where

import qualified Data.ByteString.Lazy as BS

import Debug.Trace
import Control.Lens (_Left, over)
import Data.Aeson (eitherDecode)
import Data.List.Split (splitOn)

import AST
import Scope (resolveRoot, allStrings, ScopeError)
import BC (gen, Module, BCError)

data Error
  = ParseError String
  | ScopingError ScopeError
  | BytecodeError BCError
  deriving (Show)

compile :: String -> BS.ByteString -> Either Error Module
compile rawModuleName contents = do
  let moduleName = ModuleName $ splitOn "." rawModuleName
  block <- traceShowId $ readBlock contents
  let strings = allStrings block
  resolved <- traceShowId $ readRoot strings block
  genBC moduleName strings resolved

readBlock :: BS.ByteString -> Either Error (Block 'U)
readBlock = over _Left ParseError . eitherDecode

readRoot :: [String] -> Block 'U -> Either Error (Block 'R)
readRoot strings = over _Left ScopingError . resolveRoot strings

genBC :: ModuleName -> [String] -> Block 'R -> Either Error Module
genBC moduleName strings block = over _Left BytecodeError $ gen moduleName strings block
