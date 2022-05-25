# tasty-golden-easy

Provides streamlined experience for golden/expect testing using @tasty-golden@.

Instead of declaring a file path for each golden tests, this modules provides `expect` which combined with
`declModulePath` allows creating golden test by only specifying its test name and its actual value.

Here's how it will look like:

```haskell
module Example.HelloWorld where

-- import both @declModulePath@ and @expect@
import Test.Tasty.Golden.Easy (declModulePath, expect)

import Data.ByteString.Lazy.Char8 qualified as ByteString
import Test.Tasty (TestTree)

-- unquote @declModulePath@
$(declModulePath)

-- declare your golden test
helloWorldTest :: TestTree
helloWorldTest =
  $(expect) "Print hello world" $
    (pure . ByteString.pack) "Hello world!"

-- when @helloWorldTest@ gets evaluated, it will create the golden file at
-- $HASKELL_TEST_SRC_DIR/Test/Example/goldens/HelloWorld_print_hello_world.golden
```
