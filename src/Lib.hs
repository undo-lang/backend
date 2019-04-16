module Lib
  ( compile
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.Aeson (eitherDecode)
import Control.Lens (_Left, _Right, set, over)
import Control.Monad (foldM)
import Data.Functor (($>))
import AST

compile :: BS.ByteString -> IO ()
compile contents = putStrLn $ show $ do
  block <- readBlock contents
  checkUseBeforeDeclare block []

-- XXX should we have different error types depending on where where are in the program?
data Error
  = ParseError String
  | DuplicateVariable String
  | DuplicateParameter String
  deriving (Show)

readBlock :: BS.ByteString -> Either Error Block
readBlock = over _Left ParseError . eitherDecode

-- TODO use Validation instead of Either here
-- TODO use (Source, String) instead of simply `String`, so we know if it was previously declared as a param or a var
-- XXX use `foldM_` here?
checkUseBeforeDeclare :: Block -> [String] -> Either Error [String]
checkUseBeforeDeclare (Block lines) names = foldM check names lines
  where check :: [String] -> Line -> Either Error [String]
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
reformatBlock :: Block -> Either Error Block
reformatBlock x = Right x
