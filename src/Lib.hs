{-# LANGUAGE DataKinds #-}
module Lib
  ( compile
  ) where

import qualified Data.ByteString.Lazy as BS
import Control.Lens (_Left, _Right, set, over)
import Control.Monad (foldM)
import Data.Functor (($>))
import Data.List (mapAccumL)

import Data.Aeson (eitherDecode)

import AST

compile :: BS.ByteString -> IO ()
compile contents = putStrLn $ show $ do
  block <- readBlock contents
  checkUseBeforeDeclare block emptyScope

-- XXX should we have different error types depending on where where are in the program?
data Error
  = ParseError String
  | DuplicateVariable String
  | DuplicateParameter String
  deriving (Show)

readBlock :: BS.ByteString -> Either Error (Block 'U)
readBlock = over _Left ParseError . eitherDecode

type Scope = [String]
emptyScope = []

-- TODO use Validation instead of Either here
-- TODO use mapAccumM to get `Either Error` back
--      ( https://hackage.haskell.org/package/Cabal-2.4.1.0/docs/src/Distribution.Utils.MapAccum.html )
resolveTree :: Scope -> Block 'U -> Block 'R
resolveTree scope (Block lines) = Block $ snd $ mapAccumL resolveLine scope lines
  where resolveLine :: Scope -> Line 'U -> (Scope, Line 'R)
        resolveLine scope (LineDecl (Var name)) =
          -- TODO check shadowing?
          -- TODO keep nested scopes, so we can check { var a; var a; } vs { var a; {var a;}}?
          ((name:scope), LineDecl $ Var name)

        resolveLine scope (LineDecl (Fn name params body_)) =
          let newScope = scope ++ (name:extractParams params) -- TODO check shadowing? for sure check fn name dupes
          in (newScope, LineDecl $ Fn name params (resolveTree newScope body_))

        resolveLine scope (LineDecl (Import name)) =
          -- TODO scope should be ([[String]], [String]) to keep namespaces list around, maybe record
          (undefined:scope, LineDecl $ Import name)

        resolveLine scope (LineExpr _) =
          (scope, undefined)

        -- TODO steal Either version from checkUseBeforeDeclare
        extractParams :: ParameterList -> [String]
        extractParams (ParameterList params) = extractParamName <$> params

        extractParamName :: Parameter -> String
        extractParamName (Parameter p) = p

        resolveName :: Name 'U -> Name 'R
        resolveName _ = undefined

checkUseBeforeDeclare :: Block 'U -> [String] -> Either Error [String]
checkUseBeforeDeclare (Block lines) names = foldM check names lines
  where check :: [String] -> (Line 'U) -> Either Error [String]
        check names (LineDecl (Var name)) = checkDupe name names DuplicateVariable
        -- XXX  we don't check shadowing here, that (++ names possibly has dupes). We can `nub` to remove dupes, or error
        check names (LineDecl (Fn name pr block)) = (extractParamNames pr >>= checkUseBeforeDeclare block . (++ names)) $> names
        check names (LineDecl _) = Right names
        check names (LineExpr expr) = Right names -- TODO basically the opposite of checkDupe

        extractParamNames :: ParameterList -> Either Error [String]
        extractParamNames (ParameterList params) = foldM extractParamName [] params

        extractParamName :: [String] -> Parameter -> Either Error [String]
        extractParamName xs (Parameter n) = checkDupe n xs DuplicateParameter

        checkDupe :: Eq a => a -> [a] -> (a -> b) -> Either b [a]
        checkDupe x xs lft = rightIf (x `elem` xs) (lft x) (x:xs)

-- TODO helpers file or w/e
rightIf :: Bool -> a -> b -> Either a b
rightIf True  _ v = Right v
rightIf False v _ = Left v

-- Reformats a block so that it's in this order:
-- o Variables
-- o Functions
reformatBlock :: (Block s) -> Either Error (Block s)
reformatBlock x = Right x
