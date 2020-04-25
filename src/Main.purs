module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String (joinWith, split, stripPrefix, stripSuffix, toLower, trim)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global, ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)

-- | Strip any characters that are not ascii lowercase letters or numbers.
strip :: String -> String
strip = replace (unsafeRegex "[^a-z0-9+]+" (global <> ignoreCase)) ""

-- | Strip any repeating dashes from a string.
stripDash :: String -> String
stripDash = replace (unsafeRegex "--+" global) "-"

-- | Creates a slug for some string.
-- |
-- | ```purescript
-- | >>> slugify "foo bar baz"
-- | "foo-bar-baz"
-- | >>> slugify "fo''o b%$!ar baz,  " 
-- | "foo-bar-baz"
-- | ```
slugify :: String -> String
slugify str = stripDash <<< joinWith "-" <<< map strip <<< split (Pattern " ") <<< toLower $ trim str

-- | Strips `public/` from a string.
-- |
-- | ```purescript
-- | >>> stripPublic "public/hello/world/"
-- | "hello/world/"
-- | >>> stripPublic "/hello/world/"
-- | "/hello/world/"
-- | ```
stripPublic :: String -> String
stripPublic str = case stripPrefix (Pattern "public/") str of
  Just out -> out
  Nothing -> str

-- | Fixes the slug of some string by removing the leading `/` of it and 
-- | appending a `/` if it is missing.
-- |
-- | ```purescript
-- | >>> fixSlug "/about" 
-- | "about/"
-- | >>> fixSlug "about" 
-- | "about/"
-- | >>> fixSlug "/about/" 
-- | "about/"
-- | ```
fixSlug :: String -> String
fixSlug str = case stripPrefix (Pattern "/") str, stripSuffix (Pattern "/") str of
  Just out, Just _ -> out
  Just out, Nothing -> out <> "/"
  Nothing, Just _ -> str
  _, _ -> str <> "/"
