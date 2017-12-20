module Main where

import TccCore.Lexer
import TccCore.Token

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
    defaultMain(unitTests)

minimalProgram = "int main(){}"
minimalTokens =
    [
    Identifier "int",
    Identifier "main",
    OpenParenthesis,
    CloseParenthesis,
    OpenBrace,
    CloseBrace
    ]

unitTests = testGroup "Unit tests"
  [ testCase "Minimal program" $
      assertEqual "Lexer should give correct tokens"
        minimalTokens
        (tccLex minimalProgram)

  -- the following test does not hold
  , testCase "Identifier int" $
      assertEqual "Lexer should give correct tokens"
        [Identifier "int"]
        (tccLex "int")
  ]
