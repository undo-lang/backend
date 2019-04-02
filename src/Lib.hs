module Lib
  ( compile
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.Aeson (eitherDecode)
import AST (Block)

compile :: BS.ByteString -> IO ()
compile contents = putStrLn $ show json
                        where json :: Either String Block
                              json = eitherDecode contents
