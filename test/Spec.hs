-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import qualified TestNoop
import qualified TestCfi
import qualified TestSealing

main :: IO ()
main = hspec $ do
  TestNoop.tests
  TestCfi.tests
  TestSealing.tests
  -- describe "Prelude.head" $ do
  --   it "returns the first element of a list" $ do
  --     head [23 ..] `shouldBe` (23 :: Int)

  --   it "returns the first element of an *arbitrary* list" $
  --     property $ \x xs -> head (x:xs) == (x :: Int)

  --   it "throws an exception if used with an empty list" $ do
  --     evaluate (head []) `shouldThrow` anyException
