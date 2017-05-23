import Test.Tasty
import Test.Tasty.QuickCheck -- for property testing
import Test.Tasty.HUnit -- for case testing

import ANN.Simple

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [propertyTests, unitTests]

propertyTests :: TestTree
propertyTests = testGroup "Property Tests" [
    annPropertyTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests" [
    annUnitTests]

--------------------------------------------------------------------------------
-- ARTIFICIAL NEURAL NETWORKS Testing Suite
--------------------------------------------------------------------------------

annPropertyTests :: TestTree
annPropertyTests = testGroup "Artificial Neural Network Property Tests" [
    ]

annUnitTests :: TestTree
annUnitTests = testGroup "Artificial Neural Network Unit Tests" [
    ]

--------------------------------------------------------------------------------
