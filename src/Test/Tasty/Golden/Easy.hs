{- |
Provides streamlined experience for golden/expect testing using @tasty-golden@.

Instead of declaring a file path for each golden tests, this modules provides @expect@ which combined with
@declModulePath@ allows creating golden test by only specifying its test name and its actual value.

Here's how it will look like:

@@@
module Example.HelloWorld where

-- import both @declModulePath@ and @expect@
import Test.Tasty.Golden.Easy (declModulePath, expect)

import Data.ByteString.Lazy.Char8 qualified as ByteString
import Test.Tasty (TestTree)

-- unquote @declModulePath@
\$(declModulePath)

-- declare your golden test
helloWorldTest :: TestTree
helloWorldTest =
  $(expect) "Print hello world" $
    (pure . ByteString.pack) "Hello world!"

-- when @helloWorldTest@ gets evaluated, it will create the golden file at
-- @$HASKELL_TEST_SRC_DIR/Test/Example/goldens/HelloWorld_print_hello_world.golden@
@@@
-}
module Test.Tasty.Golden.Easy (
  declModulePath,
  expect,
  expectJSON,
) where

import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (Config (confIndent), Indent (Spaces), defConfig, encodePretty')
import Data.ByteString.Lazy (ByteString)
import Data.Char (isPunctuation, isSymbol, isUpper, toLower)
import Data.List.Split (splitOn)
import Language.Haskell.TH (
  Dec,
  Exp (LitE),
  Lit (StringL),
  Loc (loc_module),
  Q,
  TExp (unType),
 )
import Language.Haskell.TH.Env (envQ)
import Language.Haskell.TH.Syntax (Quasi (qLocation))
import Language.Haskell.TH.Syntax.Compat
import System.FilePath (takeBaseName, takeDirectory, (</>))
import Test.Tasty.Golden (goldenVsString)

{- |
Captures @thisModulePath___@ then applies it to @goldenFilePath@.
-}
goldenFilePath' :: Q Exp
goldenFilePath' =
  [|
    goldenFilePath thisModulePath___
    |]

{- |
Produces a lambda expression that takes test name and test's actual.

This lambda expression is equivalent to @goldenVsString@ without its second argument.
-}
expect :: Q Exp
expect =
  [|
    \testName actual ->
      goldenVsString
        testName
        ($(goldenFilePath') testName Nothing)
        actual
    |]

{- |
Like @expect@ but accepts an @IO (ToJson a)@ as its actual.
-}
expectJSON :: Q Exp
expectJSON =
  [|
    \testName toJSON ->
      goldenVsString
        testName
        ($(goldenFilePath') testName (Just "json"))
        (jsonOf <$> toJSON)
    |]

{- |
Pretty encodes value into JSON format.
-}
jsonOf :: ToJSON a => a -> ByteString
jsonOf toJSON =
  encodePretty'
    defConfig{confIndent = Spaces 2}
    toJSON

{- |
Creates file path to the golden file.

>>> goldenFilePath "./test/Hello/World.hs" "Test Hello World!"
"./test/Hello/goldens/World_test_hello_world_.golden"
-}
goldenFilePath :: FilePath -> String -> Maybe String -> FilePath
goldenFilePath path name ext =
  takeDirectory path
    </> "goldens"
    </> takeBaseName path
    <> "_"
    <> snakeCaseOf name
    <> ".golden"
    <> maybe mempty ("." <>) ext

{- |
Turns arbitrary string into snake_case form.

>>> snakeCaseOf "Hello world! How are you?"
"hello_world__how_are_you_"
-}
snakeCaseOf :: String -> String
snakeCaseOf =
  map
    \case
      ' ' -> '_'
      x | isPunctuation x || isSymbol x -> '_'
      x | isUpper x -> toLower x
      x -> x

{- |
Declares @thisModulePath@ at in this module top level.

This quote reads test directory path from environment variable @HASKELL_TEST_SRC_DIR@. When that fail, it will fallback
to @"./test"@.

@thisModulePath@ is a relative @FilePath@ to the current module.

Function to get the current module name is by Chris Kuklewicz from
<https://stackoverflow.com/questions/5480228/how-to-get-the-current-module-name-in-haskell/5480638#5480638>.
-}
declModulePath :: Q [Dec]
declModulePath =
  [d|
    thisModulePath___ =
      ( maybe
          "./test"
          id
          $( unType <$> examineCode (envQ @FilePath "HASKELL_TEST_SRC_DIR")
           )
      )
        </> foldr
          ((</>))
          mempty
          ( splitOn
              "."
              $( fmap loc_module qLocation
                  >>= \mod -> pure (LitE (StringL mod))
               )
          )
    |]
