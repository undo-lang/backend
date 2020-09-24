{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Undo.ASTSpec where

import AST
import Data.Aeson
import Test.Hspec (describe, it, shouldBe, Spec)
import Data.ByteString.Lazy

parseBlock :: Data.ByteString.Lazy.ByteString -> Either String (Block 'U)
parseBlock = eitherDecode

--blockShouldBe x y = (parseBlock x) `shouldBe` (Right y)

spec :: Spec
spec = do
  describe "AST module" $ do
    it "is buildable" $ do
      1 `shouldBe` 1
      --"{\"type\": \"Block\"}" `blockShouldBe` Block []
      --(parseBlock "{\"type\": \"Block\"}") `shouldBe` (Right $ Block [])
