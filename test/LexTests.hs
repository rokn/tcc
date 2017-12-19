module Main where

import TccCore.Lexer

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
    defaultMain(testGroup "Lexer tests" [dummyTest, lexerTest])

dummyTest :: TestTree
dummyTest = testCase "Testing a dummy 5==5"
    (assertEqual "5 should be equal to 5" 6 3)

lexerTest :: TestTree
lexerTest = testCase "Testing a lexer"
    (assertEqual "5 should be equal to 5" 5 5)
