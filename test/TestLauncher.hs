module Main where

import Test.Tasty
import LexTests

main :: IO ()
main = do
    defaultMain(LexTests.tests)

