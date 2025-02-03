module Main where

import Lib (compile, Error)
import BC (Module)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BS
import Data.Aeson (encode)

main :: IO ()
main = getArgs >>= runCli

runCompile :: String -> FilePath -> IO (Either Error Module)
runCompile name file = compile name <$> BS.readFile file

writeModule :: String -> Module -> IO ()
writeModule name mod = do
  BS.writeFile (name ++ ".undo-bc") $ encode mod
  putStrLn $ "correctly generated " ++ name
  mempty

runCli :: [String] -> IO ()
runCli [name, file] = do
  putStrLn $ "You want to read " ++ file
  mod <- runCompile name file
  either print (writeModule name) mod
runCli _      = putStrLn "You need to pass <module name> <module AST JSON file>"
