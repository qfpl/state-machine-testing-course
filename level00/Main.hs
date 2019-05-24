module Main where

import PropertyTests (propertyTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests = testGroup "Coffee Machine - Level 00" [propertyTests]

main :: IO ()
main = defaultMain tests
