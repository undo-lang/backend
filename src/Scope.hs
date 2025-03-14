{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Scope
  ( resolveRoot
  , extractParams
  , allStrings
  , ScopeError
  ) where

import Control.Monad (foldM)
import qualified Data.Map as Map
import Control.Monad.State (StateT(..), evalStateT)
import Control.Lens
import Data.Kind (Type)
import Data.List (elemIndex)

import AST

data ScopeError
  = DuplicateVariable String
  | DuplicateImport ModuleName
  -- | DuplicateParameter String
  | NoSuchVariable String
  | NoSuchNamespace ModuleName
  | DuplicateEnum String
  -- TODO duplicate enum variant
  | StringNotComputed
  deriving (Show)

type Aliases = Map.Map String ModuleName -- TODO implement those everywhere else
type EnumVariants = Map.Map String Int -- fields

data Scope = Scope
  { _scopeNames :: [String],
    _scopeNamespaces :: [ModuleName],
    __scopeAliases :: Aliases,
    _scopeEnums :: Map.Map String EnumVariants }

$(makeLenses ''Scope)

emptyScope :: Scope
emptyScope = Scope ["MAIN"] [ModuleName ["Prelude"]] Map.empty Map.empty

addName :: String -> Scope -> Scope
addName = (scopeNames <>~) . pure

-- addNames :: [String] -> Scope -> Scope
-- addNames = (scopeNames <>~)

addNamespace :: ModuleName -> Scope -> Scope
addNamespace = (scopeNamespaces <>~) . pure

addEnum :: (String, [EnumVariant]) -> Scope -> Scope
addEnum (name, variants) = scopeEnums.at name ?~ (Map.fromList (variantize <$> variants) :: EnumVariants)
  where variantize (EnumVariant n xs) = (n, length xs)

-- from https://stackoverflow.com/questions/11652809/how-to-implement-mapaccumm
-- ( like https://hackage.haskell.org/package/Cabal-2.4.1.0/docs/src/Distribution.Utils.MapAccum.html )
-- note: use runState to keep the accum if needed
mapAccumM_ :: (Monad m, Functor m, Traversable t) => (a -> b -> m (c, a)) -> a -> t b -> m (t c)
mapAccumM_ f = flip (evalStateT . traverse (StateT . flip f))

resolveRoot :: [String] -> Block 'U -> Either ScopeError (Block 'R)
resolveRoot stringTable = resolveTree stringTable emptyScope -- XXX should probably make sure we don't shadow functions... should it?

type Resolver (s :: NameStage -> Type)
  = Scope -> s 'U -> Either ScopeError (s 'R)
type ResolverWithScope (s :: NameStage -> Type)
  = Scope -> s 'U -> Either ScopeError (s 'R, Scope)

allStrings :: Block 'U -> [String]
allStrings (Block lines) = lines^..folded._LineExpr._LitStr

resolveTree :: [String] -> Resolver Block
resolveTree allStrings scope (Block lines) = Block <$> mapAccumM_ resolveLine hoistedScope lines
  where topLevelFunctions :: [String]
        topLevelFunctions = lines^..folded._LineDecl._Fn._1

        --hoistedScope = topLevelFunctions `addNames` scope
        hoistedScope = scope

        resolveLine :: ResolverWithScope Line
        resolveLine scope (LineDecl (Var name)) =
          (LineDecl $ Var name,) <$> declName scope name

        resolveLine scope (LineDecl (Fn name params body_)) =
          -- TODO addName `name` in case it's not a global function, for local ones
          do newScope <- declNames scope $ name:extractParams params
             resolvedBody <- resolveTree allStrings newScope body_
             outerScope <- declName scope name
             pure (LineDecl $ Fn name params resolvedBody, outerScope)

        -- TODO import ADTs etc
        resolveLine scope (LineDecl (Import ns)) =
          (LineDecl $ Import ns,) <$> declNs scope ns

        resolveLine scope (LineDecl (Enum n xs)) = do
          newScope <- declEnum scope (n, xs)
          pure (LineDecl (Enum n xs), newScope)

        resolveLine scope (LineExpr expr) =
          (, scope) . LineExpr <$> resolveExpr scope expr

        resolveExpr :: Resolver Expr
        resolveExpr scope (CallExpr fn args) =
          CallExpr <$> resolveExpr scope fn <*> traverse (resolveExpr scope) args

        resolveExpr scope (LoopExpr cond blk) = -- TODO extract variable decl(s) from `cond`?
          LoopExpr <$> resolveExpr scope cond <*> resolveTree allStrings scope blk

        resolveExpr scope (ConditionalExpr cond then_ else_) = -- TODO extract variable decl(s) from `cond`?
          ConditionalExpr <$> resolveExpr scope cond <*> resolveTree allStrings scope then_ <*> resolveTree allStrings scope else_

        resolveExpr scope (NameExpr n) =
          NameExpr <$> resolveName scope n

        resolveExpr _ (LitStr s) =
          case s `elemIndex` allStrings of
            Just i -> Right $ InternedStr i
            Nothing -> Left StringNotComputed

        resolveExpr _ (LitNum i) =
          Right $ LitNum i

        resolveExpr scope (MatchExpr topic bs) =
          MatchExpr <$> resolveExpr scope topic <*> traverse (resolveMatchBranch scope) bs

        resolveMatchBranch :: Resolver MatchBranch
        resolveMatchBranch scope (MatchBranch s b) = MatchBranch s <$> resolveTree allStrings scope b

        resolveName :: Resolver Name
        resolveName scope (Unresolved s) =
          bimap NoSuchVariable Local $ eitherIf (`elem` topLevelFunctions ++ scope^.scopeNames) s

        resolveName scope (Prefixed m s) = -- TODO Aliases
          bimap NoSuchNamespace (`Namespaced` s) $ eitherIf (`elem` scope^.scopeNamespaces) m

        declNs :: Scope -> ModuleName -> Either ScopeError Scope -- TODO if an Alias already exists with that name, error
        declNs scope = bimap DuplicateImport (`addNamespace` scope) . eitherUnless (`elem` scope^.scopeNamespaces)

        declName :: Scope -> String -> Either ScopeError Scope
        declName scope = bimap DuplicateVariable (`addName` scope) . eitherUnless (`elem` scope^.scopeNames)

        declNames :: Scope -> [String] -> Either ScopeError Scope
        declNames = foldM declName

        declEnum :: Scope -> (String, [EnumVariant]) -> Either ScopeError Scope
        declEnum scope = bimap (DuplicateEnum . fst) (`addEnum` scope) . eitherIf (\(n, _) -> Map.member n (scope^.scopeEnums))

eitherIf :: (a -> Bool) -> a -> Either a a
eitherIf comp a = if comp a then Right a else Left a

eitherUnless :: (a -> Bool) -> a -> Either a a
eitherUnless comp a = if comp a then Left a else Right a

extractParams :: ParameterList -> [String]
extractParams (ParameterList params) = unParameter <$> params
