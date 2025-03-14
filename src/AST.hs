{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}

module AST where

import GHC.Generics
import Control.Monad (guard)
import Data.Foldable (asum)
import Data.Kind (Type)
import Data.Aeson --(FromJSON(..), (.:), withObject, Object, Object(..))
import Control.Lens.Prism (prism', Prism')
import Control.Lens.Wrapped (Wrapped)
import Control.Lens.Plated (Plated(..))
import Control.Lens.Type (Traversal') -- TMP

newtype ModuleName = ModuleName [String]
  deriving (Generic, FromJSON, ToJSON, Eq, Show)

-- TODO refactor string table to use this GADT scheme (so we know indices are correct by construction)
data NameStage = U | R

data Name :: NameStage -> Type where
  Unresolved :: String -> Name 'U
  Prefixed   :: ModuleName -> String -> Name 'U
  Local      :: String -> Name 'R
  Namespaced :: ModuleName -> String -> Name 'R
deriving instance Show (Name s)
deriving instance Eq (Name s)

_Local :: Prism' (Name 'R) String
_Local = prism' Local (\case Local n -> Just n
                             _        -> Nothing)

_Namespaced :: Prism' (Name 'R) (ModuleName, String)
_Namespaced = prism' (uncurry Namespaced) (\case (Namespaced m s) -> Just (m, s)
                                                 _        -> Nothing)

instance FromJSON (Name 'U) where
  parseJSON = withObject "name" $ \o -> do
    type_ <- o .: "type"
    case type_ of
      "Unqualified" -> Unresolved <$> o .: "name"
      "Qualified"   -> Prefixed <$> o .: "module" <*> o .: "name"
      _             -> fail $ "Unhandled name type: " ++ type_

data MatchSubject
  = MatchSubjectConstructor String [MatchSubject]
  deriving (Eq, Show)

instance FromJSON MatchSubject where
  parseJSON = withObject "match subject" $ \o -> do
    type_ <- o .: "type"
    case type_ of
      "Constructor" -> MatchSubjectConstructor <$> o .: "constructor" <*> o .: "sub"
      _ -> fail $ "Unknown match subject type: " ++ type_

data MatchBranch s = MatchBranch MatchSubject (Block s)
  deriving (Eq, Show)

instance FromJSON (MatchBranch 'U) where
  parseJSON = withObject "match branch" $ \o -> do
    MatchBranch <$> o .: "subject" <*> o .: "block"

data Expr s where
  LitStr :: String -> Expr 'U
  InternedStr :: Int -> Expr 'R
  LitNum :: Int -> Expr s
  CallExpr :: (Expr s) -> [Expr s] -> Expr s
  LoopExpr :: (Expr s) -> (Block s) -> Expr s
  ConditionalExpr :: (Expr s) -> (Block s) -> (Block s) -> Expr s
  NameExpr :: (Name s) -> Expr s
  MatchExpr :: Expr s -> [MatchBranch s] -> Expr s
deriving instance Show (Expr s)
deriving instance Eq (Expr s)

_LitStr :: Prism' (Expr 'U) String
_LitStr = prism' LitStr (\case LitStr s -> Just s
                               _        -> Nothing)

_CallExpr :: Prism' (Expr s) (Expr s, [Expr s])
_CallExpr = prism' (uncurry CallExpr) (\case (CallExpr c a) -> Just (c, a)
                                             _ -> Nothing)

_NameExpr :: Prism' (Expr s) (Name s)
_NameExpr = prism' NameExpr (\case NameExpr n -> Just n
                                   _          -> Nothing)

instance FromJSON (Expr 'U) where
  parseJSON = withObject "expr" $ \o -> asum [
    NameExpr <$> parseJSON (Object o),
    do
      type_ <- o .: "type"
      case type_ of
        "String" -> LitStr <$> o .: "value"
        "Num"    -> LitNum <$> o .: "value"
        "Call"   -> CallExpr <$> o .: "fn" <*> o .: "argument"
        "Conditional" -> ConditionalExpr <$> o .: "condition" <*> o .: "then" <*> o .: "else"
        "Match" -> MatchExpr <$> o .: "topic" <*> o .: "branch"
        _        -> fail $ "Unknown expr type: " ++ type_
    ]

newtype Parameter = Parameter {unParameter :: String}
  deriving (Generic, Eq, Show)

instance FromJSON Parameter where
  parseJSON = withObject "parameter" $ \o -> do
    type_ <- o .: "type"
    guard ((type_ :: String) == "Parameter")
    Parameter <$> o .: "name"

newtype ParameterList = ParameterList [Parameter]
  deriving (Generic, FromJSON, Eq, Show)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

data EnumVariant = EnumVariant String [String]
  deriving (Eq, Show)

instance FromJSON EnumVariant where
  parseJSON = withObject "json variant" $ \o ->
    EnumVariant <$> o .: "name" <*> o .: "parameter"

data Decl s
  = Import ModuleName -- TODO other forms of import
  | Var String
  | Fn String ParameterList (Block s)
  | Enum String [EnumVariant]
  deriving (Eq, Show)

_Import :: Prism' (Decl s) ModuleName
_Import = prism' Import (\case Import m -> Just m
                               _        -> Nothing)

_Var :: Prism' (Decl s) String
_Var = prism' Var (\case Var s -> Just s
                         _     -> Nothing)

_Fn :: Prism' (Decl s) (String, ParameterList, Block s)
_Fn = prism' (uncurry3 Fn) (\case (Fn s p b) -> Just (s, p, b)
                                  _          -> Nothing)

_Enum :: Prism' (Decl s) (String, [EnumVariant])
_Enum = prism' (uncurry Enum) (\case (Enum n xs) -> Just (n, xs)
                                     _ -> Nothing)

instance Plated (Decl 'R) where
  plate f (Fn s p b) = Fn s p <$> plateBlock f b
    where plateBlock :: Traversal' (Block s) (Decl s)
          plateBlock f (Block xs) = Block <$> traverse (plateElem f) xs

          plateElem :: Traversal' (Line s) (Decl s)
          plateElem f (LineDecl d) = LineDecl <$> f d
          plateElem f (LineExpr e) = LineExpr <$> plateExpr f e

          plateExpr :: Traversal' (Expr s) (Decl s)
          plateExpr f (CallExpr e xs) = CallExpr <$> plateExpr f e <*> traverse (plateExpr f) xs
          plateExpr f (LoopExpr e b) = LoopExpr <$> plateExpr f e <*> plateBlock f b
          plateExpr f (ConditionalExpr c t e) = ConditionalExpr <$> plateExpr f c <*> plateBlock f t <*> plateBlock f e
          plateExpr _ e = pure e
  plate _ decl = pure decl

instance Plated (Expr s) where
  plate = plateExpr
    where plateBlock :: Traversal' (Block s) (Expr s)
          plateBlock f (Block xs) = Block <$> traverse (plateElem f) xs

          plateDecl :: Traversal' (Decl s) (Expr s)
          plateDecl f (Fn s p b) = Fn s p <$> plateBlock f b
          plateDecl _ d = pure d

          plateElem :: Traversal' (Line s) (Expr s)
          plateElem f (LineDecl d) = LineDecl <$> plateDecl f d
          plateElem f (LineExpr e) = LineExpr <$> f e

          plateExpr :: Traversal' (Expr s) (Expr s)
          plateExpr f (CallExpr e xs) = CallExpr <$> f e <*> traverse f xs
          plateExpr f (LoopExpr e b) = LoopExpr <$> f e <*> plateBlock f b
          plateExpr f (ConditionalExpr c t e) = ConditionalExpr <$> f c <*> plateBlock f t <*> plateBlock f e
          plateExpr _ e = pure e

instance FromJSON (Decl 'U) where
  parseJSON = withObject "decl" $ \o -> do
    type_ <- o .: "type"
    case type_ of
      "Import" -> Import <$> o .: "value"
      "Var"    -> Var <$> o .: "name"
      "Fn"     -> Fn <$> o .: "name" <*> o .: "parameter" <*> o .: "body"
      "Enum"   -> Enum <$> o .: "name" <*> o .: "variant"
      _        -> fail $ "Unknown decl type: " ++ type_

data Line s = LineExpr (Expr s) | LineDecl (Decl s)
  deriving (Eq, Show)

instance FromJSON (Line 'U) where
  parseJSON = withObject "line" $ \o -> asum [
      LineExpr <$> parseJSON (Object o),
      LineDecl <$> parseJSON (Object o)
    ]

_LineExpr :: Prism' (Line s) (Expr s)
_LineExpr = prism' LineExpr (\case LineExpr e -> Just e
                                   _          -> Nothing)

_LineDecl :: Prism' (Line s) (Decl s)
_LineDecl = prism' LineDecl (\case LineDecl e -> Just e
                                   _          -> Nothing)

newtype Block s = Block [Line s]
  deriving (Eq, Show, Generic)

instance Wrapped (Block s)

instance FromJSON (Block 'U) where
  parseJSON = withObject "block" $ \o -> do
    type_ <- o .: "type"
    guard ((type_ :: String) == "Block")
    Block <$> o .: "body"
