module Main where

import Data.Foldable (traverse_)
import Example.HelloWorld qualified as HelloWorld
import Example.ToJSON qualified as ToJSON
import System.Environment (getArgs)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
  printArgs
    >> ( defaultMain $
          testGroup
            "Tests"
            [ examples
            ]
       )

examples =
  testGroup
    "Examples"
    [ HelloWorld.helloWorldTest
    , ToJSON.helloWorldTest
    ]

-- | Prints argument passed to the test driver.
printArgs :: IO ()
printArgs =
  putStrLn "Test arguments:"
    >> getArgs
    >>= ( \xs ->
            traverse_
              (\x -> putStrLn $ "  " <> x)
              xs
        )
    >> putStrLn ""
