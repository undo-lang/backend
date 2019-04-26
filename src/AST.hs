{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
-- LANGUAGE BlockArguments

module AST where

import GHC.Generics
import Control.Monad (guard)
import Data.Foldable (asum)
import Data.Aeson --(FromJSON(..), (.:), withObject, Object, Object(..))

newtype ModuleName = ModuleName [String]
  deriving (Generic, FromJSON, Show)

data NameStage = U | R
data Name :: NameStage -> * where
  Unresolved :: String -> Name 'U
  Variable   :: String -> Name 'R
  -- TODO other names
deriving instance Show (Name s)

instance FromJSON (Name 'U) where
  parseJSON = withObject "name" $ \o -> do
    type_ <- o .: "type"
    case type_ of
      "Unqualified" -> Unresolved <$> o .: "name"
      "Qualified"   -> fail "Unimplemented: qualified names"
      _             -> fail $ "Unhandled name type: " ++ type_

data Expr s
  = LitStr String
  | LitNum Int
  | CallExpr (Expr s) [Expr s]
  | LoopExpr (Expr s) (Block s)
  | ConditionalExpr (Expr s) (Block s) (Block s)
  | NameExpr (Name s)
  deriving (Show)

instance FromJSON (Expr 'U) where
  parseJSON = withObject "expr" $ \o -> asum [
    NameExpr <$> parseJSON (Object o),
    do
      type_ <- o .: "type"
      case type_ of
        "String" -> LitStr <$> o .: "value"
        "Num"    -> LitNum <$> o .: "value"
        "Call"   -> CallExpr <$> o .: "fn" <*> o .: "argument"
        _        -> fail $ "Unknown expr type: " ++ type_
    ]

newtype Parameter = Parameter String
  deriving (Generic, FromJSON, Show)

newtype ParameterList = ParameterList [Parameter]
  deriving (Generic, FromJSON, Show)

data Decl s
  = Import ModuleName
  | Var String
  | Fn String ParameterList (Block s)
  deriving (Show)

instance FromJSON (Decl 'U) where
  parseJSON = withObject "decl" $ \o -> do
    type_ <- o .: "type"
    case type_ of
      "Import" -> Import <$> o .: "value"
      "Var"    -> Var <$> o .: "name"
      "Fn"     -> Fn <$> o .: "fn" <*> o .: "parameter" <*> o .: "block"
      _        -> fail $ "Unknown decl type: " ++ type_

data Line s = LineExpr (Expr s) | LineDecl (Decl s)
  deriving (Show)

instance FromJSON (Line 'U) where
  parseJSON = withObject "line" $ \o -> asum [
      LineExpr <$> parseJSON (Object o),
      LineDecl <$> parseJSON (Object o)
    ]

newtype Block s = Block [Line s]
  deriving (Show)

instance FromJSON (Block 'U) where
  parseJSON = withObject "block" $ \o -> do
    type_ <- o .: "type"
    guard ((type_ :: String) == "Block")
    Block <$> o .: "body"
