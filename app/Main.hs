module Main where

import Lib (compile)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = getArgs >>= runCli

runCompile :: String -> FilePath -> IO ()
runCompile name file = BS.readFile file >>= compile name

runCli :: [String] -> IO ()
runCli [name, file] = do
  putStrLn $ "You want to read " ++ file
  runCompile name file
  mempty
runCli _      = putStrLn "You need to pass `module name` `module AST JSON file`"
