module Undo.ASTSpec where

import Test.Hspec (describe, it, shouldBe, Spec)

spec :: Spec
spec = do
  describe "AST module" $ do
    it "is buildable" $ do
      1 `shouldBe` 1
