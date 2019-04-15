module Lib
  ( compile
  ) where

import qualified Data.ByteString.Lazy as BS
import Data.Aeson (eitherDecode)
import Control.Lens (_Left, _Right, set, over)
import Data.Functor (($>))
import AST

compile :: BS.ByteString -> IO ()
compile contents = putStrLn $ show $ do
  block <- readBlock contents
  checkUseBeforeDeclare block []

newtype Error = Error String
  deriving (Show)

readBlock :: BS.ByteString -> Either Error Block
readBlock = over _Left Error . eitherDecode

traverseAccum :: (a -> b -> Either Error b) -> [a] -> b -> Either Error b
traverseAccum fn (x:xs) val = fn x val >>= traverseAccum fn xs
traverseAccum _  []     val = return val

traverseAccumT :: Monoid b => (a -> b -> Either Error b) -> [a] -> Either Error b
traverseAccumT fn xs = traverseAccum fn xs mempty

-- TODO use Validation instead of Either here
-- TODO use (Source, String) instead of simply `String`, so we know if it was previously declared as a param or a var
checkUseBeforeDeclare :: Block -> [String] -> Either Error [String]
checkUseBeforeDeclare (Block lines) names = traverseAccum check lines names
  where check :: Line -> [String] -> Either Error [String]
        check (LineDecl (Var name)) names = checkDupe name names dupeVariable
        check (LineDecl (Fn name pr block)) names = (extractParamNames pr >>= checkUseBeforeDeclare block . (++ names)) $> names
        -- XXX we don't check shadowing here, that (++ names possibly has dupes). We can `nub` to remove dupes, or error
        check (LineDecl _) names = Right names
        check (LineExpr expr) names = Right names -- TODO basically the opposite of checkDupe

        extractParamNames :: ParameterList -> Either Error [String]
        extractParamNames (ParameterList params) = traverseAccum extractParamName params []

        extractParamName :: Parameter -> [String] -> Either Error [String]
        extractParamName (Parameter n) xs = checkDupe n xs dupeParam

        checkDupe :: Eq a => a -> [a] -> (a -> b) -> Either b [a]
        checkDupe x xs lft = rightIf (x `elem` xs) (lft x) (x:xs)

-- TODO ctor of Error
dupeVariable :: String -> Error
dupeVariable = Error . ("Duplicate variable: " ++)

-- TODO ctor of Error
dupeParam :: String -> Error
dupeParam = Error . ("Duplicate parameter: " ++)

-- TODO helpers file or w/e
rightIf :: Bool -> a -> b -> Either a b
rightIf True  _ v = Right v
rightIf False v _ = Left v

-- Reformats a block so that it's in this order:
-- o Variables
-- o Functions
reformatBlock :: Block -> Either Error Block
reformatBlock x = Right x
