module Main where

import Prelude
import Data.String (toLower, joinWith, split, trim)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global, ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)

-- import Effect (Effect)
-- import Effect.Console (log)
strip :: String -> String
strip = replace (unsafeRegex "[^a-z0-9+]+" (global <> ignoreCase)) ""

stripDash :: String -> String
stripDash = replace (unsafeRegex "--+" global) "-"

slugify :: String -> String
slugify str = stripDash <<< joinWith "-" <<< map strip <<< split (Pattern " ") <<< toLower $ trim str
