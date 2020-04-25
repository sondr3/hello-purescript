module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (slugify, stripPublic, fixSlug)
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
            slugify "foo bar baz" `shouldEqual` "foo-bar-baz"
          it "removes trailing whitespace" do
            slugify "   foo bar   baz  " `shouldEqual` "foo-bar-baz"
          it "removes illegal characters" do
            slugify "foo, bar. baz!" `shouldEqual` "foo-bar-baz"
            slugify "fo''o b%$!ar baz,  " `shouldEqual` "foo-bar-baz"
          it "strips dashes correctly" do
            slugify "-foo-- bar, baz" `shouldEqual` "foo-bar-baz"
          it "strips" do
            slugify "Hello, world! It's a glorious" `shouldEqual` "hello-world-its-a-glorious"
            slugify "this_ IS a % of $dollars" `shouldEqual` "this-is-a-of-dollars"
            slugify "1 is equal == to ---3" `shouldEqual` "1-is-equal-to-3"
        describe "strip public" do
          it "strips `public/`" do
            stripPublic "public/assets/index.be69cc27.css" `shouldEqual` "assets/index.be69cc27.css"
            stripPublic "public/about/index.html" `shouldEqual` "about/index.html"
            stripPublic "public/index.html" `shouldEqual` "index.html"
            stripPublic "public/" `shouldEqual` ""
          it "does not strip `/public/`" do
            stripPublic "/public/" `shouldEqual` "/public/"
        describe "fix slug" do
          it "strip beginning `/`" do
            fixSlug "/about" `shouldEqual` "about/"
            fixSlug "/" `shouldEqual` ""
          it "appends ending `/`" do
            fixSlug "about" `shouldEqual` "about/"
          it "strips beginning `/` and appends `/`" do
            fixSlug "/about/" `shouldEqual` "about/"
            fixSlug "about/" `shouldEqual` "about/"
