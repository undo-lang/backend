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
import Data.Aeson --(FromJSON(..), (.:), withObject, Object, Object(..))
import Data.Foldable (asum)

newtype ModuleName = ModuleName [String]
  deriving (Generic, FromJSON)
data Name
  = QualifiedName ModuleName String
  | UnqualifiedName String

instance FromJSON Name where
  parseJSON = withObject "name" $ \o -> do
    type_ <- o .: "type"
    case type_ of
      "Unqualified" -> UnqualifiedName <$> o .: "name"
      "Qualified"   -> fail "Unimplemented: qualified names"
      _            -> fail $ "Unhandled name type: " ++ type_

data Expr
  = LitStr String
  | LitNum Int
  | CallExpr Expr [Expr]
  | LoopExpr Expr Block
  | ConditionalExpr Expr Block Block

instance FromJSON Expr where
  parseJSON = withObject "expr" $ \o -> do
    type_ <- o .: "type"
    case type_ of
      "String" -> LitStr <$> o .: "value"
      "Num"    -> LitNum <$> o .: "value"
      "Call"   -> CallExpr <$> o .: "fn" <*> o .: "argument"
      _        -> fail $ "Unknown expr type: " ++ type_

newtype Parameter = Parameter String -- newtype the String?
  deriving (Generic, FromJSON)
newtype ParameterList = ParameterList [Parameter]
  deriving (Generic, FromJSON)

data Decl
  = Import ModuleName
  | Var String
  | Fn String ParameterList Block
--instance FromJSON Decl where
--  parseJSON = withObject "decl" $ \o -> do
--    type_ <- o .: "type"
--    case type_ of
--      "Import" -> Import <$> o .: "value" -- TODO check
--      "Var"    -> Var <$> o .: "name"     -- TODO check
--      "Fn"     -> Fn <$> o .: "fn" <*> o .: "parameter" <*> o .: "body" -- TODO check

data Line = LineExpr Expr | LineDecl Decl
--instance FromJSON Line where
--  parseJSON = withObject "line" $ \o -> asum [
--      LineExpr <$> parseJSON (Object o)
--      LineDecl <$> parseJSON (Object o)
--    ]
data Block = Block [Line] -- todo takewhile isDecl + error if contains LineDecl in _2
newtype Block = Block [Line]
  deriving (Generic, FromJSON)
