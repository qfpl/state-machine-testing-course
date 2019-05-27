module Main where

import PropertyTests (propertyTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests = testGroup "Warm up - Level 00" [propertyTests]

main :: IO ()
main = defaultMain tests
