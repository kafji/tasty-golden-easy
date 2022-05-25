{-# LANGUAGE DeriveAnyClass #-}

module Example.ToJSON where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Test.Tasty (TestTree)
import Test.Tasty.Golden.Easy (declModulePath, expectJSON)

$(declModulePath)

data Data = Data
  { greeting :: String
  , question :: String
  }
  deriving (Generic, ToJSON)

helloWorldTest :: TestTree
helloWorldTest =
  $(expectJSON) "Hello world data" do
    pure $ Data "Hello world" "How are you?"
