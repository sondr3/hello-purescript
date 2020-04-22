module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (slugify)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "slugify" do
          it "replaces simple strings" do
            (slugify "foo bar baz") `shouldEqual` "foo-bar-baz"
          it "removes trailing whitespace" do
            (slugify "   foo bar   baz  ") `shouldEqual` "foo-bar-baz"
          it "removes illegal characters" do
            (slugify "foo, bar. baz!") `shouldEqual` "foo-bar-baz"
            (slugify "fo''o b%$!ar baz,  ") `shouldEqual` "foo-bar-baz"
