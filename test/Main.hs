module Main where

import Example.HelloWorld
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ examples
      ]

examples =
  testGroup
    "Examples"
    [ helloWorldTest
    ]
