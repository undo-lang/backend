{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Scope
  ( resolveRoot
  , ScopeError
  , extractParams
  ) where

import Control.Monad (foldM)
import qualified Data.Map as Map
import Data.Traversable (traverse)
import Control.Monad.State (StateT(..), evalStateT, runStateT)
import Control.Lens
import Debug.Trace

import AST

data ScopeError
  = DuplicateVariable String
  | DuplicateImport ModuleName
  -- | DuplicateParameter String
  | NoSuchVariable String
  | NoSuchNamespace ModuleName
  deriving (Show)

type Aliases = Map.Map String ModuleName -- TODO implement those everywhere else
type Scope = ([String], [ModuleName], Aliases)
emptyScope = (["MAIN"], [ModuleName ["Prelude"]], Map.empty)

names :: Lens' Scope [String]
names = _1
namespaces :: Lens' Scope [ModuleName]
namespaces = _2
aliases :: Lens' Scope Aliases
aliases = _3

addName = (names <>~) . pure
addNames = (names <>~)
addNamespace = (namespaces <>~) . pure

-- from https://stackoverflow.com/questions/11652809/how-to-implement-mapaccumm
-- ( like https://hackage.haskell.org/package/Cabal-2.4.1.0/docs/src/Distribution.Utils.MapAccum.html )
-- note: use runState to keep the accum if needed
mapAccumM_ :: (Monad m, Functor m, Traversable t) => (a -> b -> m (c, a)) -> a -> t b -> m (t c)
mapAccumM_ f = flip (evalStateT . (traverse (StateT . (flip f))))

resolveRoot :: Block 'U -> Either ScopeError (Block 'R)
resolveRoot = resolveTree emptyScope -- XXX should probably make sure we don't shadow functions... should it?

type Resolver (s :: NameStage -> *)
  = Scope -> s 'U -> Either ScopeError (s 'R)
type ResolverWithScope (s :: NameStage -> *)
  = Scope -> s 'U -> Either ScopeError (s 'R, Scope)

resolveTree :: Resolver Block
resolveTree scope (Block lines) = Block <$> mapAccumM_ resolveLine scope lines
  where topLevelFunctions :: [String]
        topLevelFunctions = lines^..folded._LineDecl._Fn._1

        hoistedScope = topLevelFunctions `addNames` scope

        resolveLine :: ResolverWithScope Line
        resolveLine scope (LineDecl (Var name)) =
          (LineDecl $ Var name,) <$> declName scope name

        resolveLine scope (LineDecl (Fn name params body_)) =
          do newScope <- declNames scope (name:extractParams params)
             resolvedBody <- resolveTree newScope body_
             outerScope <- declName scope name
             pure $ (LineDecl $ Fn name params resolvedBody, outerScope)

        resolveLine scope (LineDecl (Import ns)) =
          (LineDecl $ Import ns,) <$> declNs scope ns

        resolveLine scope (LineExpr expr) =
          (, scope) . LineExpr <$> resolveExpr scope expr

        resolveExpr :: Resolver Expr
        resolveExpr scope (CallExpr fn args) =
          CallExpr <$> resolveExpr scope fn <*> (sequence $ resolveExpr scope <$> args)

        resolveExpr scope (LoopExpr cond blk) = -- TODO extract variable decl(s) from `cond`?
          LoopExpr <$> resolveExpr scope cond <*> resolveTree scope blk

        resolveExpr scope (ConditionalExpr cond then_ else_) = -- TODO extract variable decl(s) from `cond`?
          ConditionalExpr <$> resolveExpr scope cond <*> resolveTree scope then_ <*> resolveTree scope else_

        resolveExpr scope (NameExpr n) =
          NameExpr <$> resolveName scope n

        resolveExpr _ (LitStr s) =
          Right $ LitStr s

        resolveExpr _ (LitNum i) =
          Right $ LitNum i

        resolveName :: Resolver Name
        resolveName scope (Unresolved s) =
          bimap NoSuchVariable Local $ eitherIf (`elem` topLevelFunctions ++ scope^.names) s

        resolveName scope (Prefixed m s) = -- TODO Aliases
          bimap NoSuchNamespace (flip Namespaced s) $ eitherIf (`elem` scope^.namespaces) m

        declNs :: Scope -> ModuleName -> Either ScopeError Scope -- TODO if an Alias already exists with that name, error
        declNs scope = bimap DuplicateImport (`addNamespace` scope) . eitherUnless (`elem` scope^.namespaces)

        declName :: Scope -> String -> Either ScopeError Scope
        declName scope = bimap DuplicateVariable (`addName` scope) . eitherUnless (`elem` scope^.names)

        declNames :: Scope -> [String] -> Either ScopeError Scope
        declNames scope names = foldM declName scope names

eitherIf :: (a -> Bool) -> a -> Either a a
eitherIf comp a = if comp a then Right a else Left a

eitherUnless :: (a -> Bool) -> a -> Either a a
eitherUnless comp a = if comp a then Left a else Right a

extractParams :: ParameterList -> [String]
extractParams (ParameterList params) = unParameter <$> params
  where unParameter :: Parameter -> String
        unParameter (Parameter p) = p
