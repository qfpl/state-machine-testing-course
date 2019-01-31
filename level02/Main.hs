module Main where

import CoffeeMachineTests (stateMachineTests)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests = testGroup "Coffee Machine - Level 02" [stateMachineTests]

main :: IO ()
main = defaultMain tests
