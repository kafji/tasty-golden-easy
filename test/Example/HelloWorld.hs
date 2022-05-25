module Example.HelloWorld where

import Data.ByteString.Lazy.Char8 qualified as ByteString
import Test.Tasty (TestTree)
import Test.Tasty.Golden.Easy (declModulePath, expect)

$(declModulePath)

helloWorldTest :: TestTree
helloWorldTest =
  $(expect) "Print hello world" $
    (pure . ByteString.pack) "Hello world!"
