module Main where

import Example.HelloWorld qualified as HelloWorld
import Example.ToJSON qualified as ToJSON
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
    [ HelloWorld.helloWorldTest
    , ToJSON.helloWorldTest
    ]
