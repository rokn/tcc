module Main where

import Test.Tasty
import LexTests
import ParseTests

main :: IO ()
main = do
    defaultMain(testGroup "Tests" [LexTests.tests, ParseTests.tests])

