module Main where

import Lib (compile)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= runCli

runCompile :: FilePath -> IO ()
runCompile file = readFile file >>= compile

runCli :: [String] -> IO ()
runCli [file] = do
  putStrLn $ "You want to read " ++ file
  runCompile file
  mempty
runCli _      = putStrLn "You need to specify a file to read!"
