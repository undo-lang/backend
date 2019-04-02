{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- LANGUAGE BlockArguments

module AST
--  (
--    ..
--  )
  where

import GHC.Generics
import Control.Monad (guard)
import Data.Foldable (asum)
import Data.Aeson --(FromJSON(..), (.:), withObject, Object, Object(..))

newtype ModuleName = ModuleName [String]
  deriving (Generic, FromJSON, Show)
data Name
  = QualifiedName ModuleName String
  | UnqualifiedName String
  deriving (Show)

instance FromJSON Name where
  parseJSON = withObject "name" $ \o -> do
    type_ <- o .: "type"
    case type_ of
      "Unqualified" -> UnqualifiedName <$> o .: "name"
      "Qualified"   -> fail "Unimplemented: qualified names"
      _             -> fail $ "Unhandled name type: " ++ type_

data Expr
  = LitStr String
  | LitNum Int
  | CallExpr Expr [Expr]
  | LoopExpr Expr Block
  | ConditionalExpr Expr Block Block
  deriving (Show)

instance FromJSON Expr where
  parseJSON = withObject "expr" $ \o -> do
    type_ <- o .: "type"
    case type_ of
      "String" -> LitStr <$> o .: "value"
      "Num"    -> LitNum <$> o .: "value"
      "Call"   -> CallExpr <$> o .: "fn" <*> o .: "argument"
      _        -> fail $ "Unknown expr type: " ++ type_

newtype Parameter = Parameter String -- newtype the String?
  deriving (Generic, FromJSON, Show)

newtype ParameterList = ParameterList [Parameter]
  deriving (Generic, FromJSON, Show)

data Decl
  = Import ModuleName
  | Var String
  | Fn String ParameterList Block
  deriving (Show)

instance FromJSON Decl where
  parseJSON = withObject "decl" $ \o -> do
    type_ <- o .: "type"
    case type_ of
      "Import" -> Import <$> o .: "value" -- TODO check
      "Var"    -> Var <$> o .: "name"     -- TODO check
      "Fn"     -> Fn <$> o .: "fn" <*> o .: "parameter" <*> o .: "block"
      _        -> fail $ "Unknown decl type: " ++ type_

data Line = LineExpr Expr | LineDecl Decl
  deriving (Show)

instance FromJSON Line where
  parseJSON = withObject "line" $ \o -> asum [
      LineExpr <$> parseJSON (Object o),
      LineDecl <$> parseJSON (Object o)
    ]

newtype Block = Block [Line]
  deriving (Show)

instance FromJSON Block where
  parseJSON = withObject "block" $ \o -> do
    type_ <- o .: "type"
    guard ((type_ :: String) == "Block")
    Block <$> o .: "body"
