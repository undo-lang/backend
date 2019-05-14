{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Scope
  ( resolveRoot
  , ScopeError
  ) where

import Control.Monad (foldM)
import Data.List (mapAccumL)
import qualified Data.Map as Map
import Data.Functor (($>))
import Data.Traversable (traverse)
import Control.Monad.State (StateT(..), evalStateT)

import Control.Lens

import AST

data ScopeError
  = DuplicateVariable String
  | DuplicateImport ModuleName
  -- | DuplicateParameter String
  deriving (Show)

type Aliases = Map.Map String ModuleName -- TODO implement those everywhere else
type Scope = ([String], [ModuleName], Aliases)
emptyScope = ([], [], Map.empty)

names :: Lens' Scope [String]
names = _1
namespaces :: Lens' Scope [ModuleName]
namespaces = _2
aliases :: Lens' Scope Aliases
aliases = _3

addName = (names <>~) . pure
addNamespace = (namespaces <>~) . pure

-- from https://stackoverflow.com/questions/11652809/how-to-implement-mapaccumm
-- ( like https://hackage.haskell.org/package/Cabal-2.4.1.0/docs/src/Distribution.Utils.MapAccum.html )
-- note: use runState to keep the accum if needed
mapAccumM :: (Monad m, Functor m, Traversable t) => (a -> b -> m (c, a)) -> a -> t b -> m (t c)
mapAccumM f = flip (evalStateT . (traverse (StateT . (flip f))))

resolveRoot :: Block 'U -> Either ScopeError (Block 'R)
resolveRoot = resolveTree emptyScope

resolveTree :: Scope -> Block 'U -> Either ScopeError (Block 'R)
resolveTree scope (Block lines) = Block <$> mapAccumM resolveLine scope lines
  where resolveLine :: Scope -> Line 'U -> Either ScopeError (Line 'R, Scope)
        resolveLine scope (LineDecl (Var name)) =
          (LineDecl $ Var name,) <$> declName scope name

        resolveLine scope (LineDecl (Fn name params body_)) =
          do newScope <- declNames scope (name:extractParams params)
             resolvedBody <- resolveTree newScope body_
             outerScope <- declName scope name
             pure $ (LineDecl $ Fn name params resolvedBody, outerScope)

        resolveLine scope (LineDecl (Import ns)) =
          (LineDecl $ Import ns,) <$> declNs scope ns

        declNs :: Scope -> ModuleName -> Either ScopeError Scope
        declNs scope = bimap DuplicateImport (`addNamespace` scope) . eitherIf (`elem` scope^.namespaces)

        declName :: Scope -> String -> Either ScopeError Scope
        declName scope = bimap DuplicateVariable (`addName` scope) . eitherIf (`elem` scope^.names)

        declNames :: Scope -> [String] -> Either ScopeError Scope
        declNames scope names = foldM declName scope names

eitherIf :: (a -> Bool) -> a -> Either a a
eitherIf comp a = if comp a then Right a else Left a

extractParams :: ParameterList -> [String]
extractParams (ParameterList params) = unParameter <$> params
  where unParameter :: Parameter -> String
        unParameter (Parameter p) = p
