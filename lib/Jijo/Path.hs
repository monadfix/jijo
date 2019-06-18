{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JSONPath definition and rendering.
module Jijo.Path
  ( -- * Paths
    JPath(..),
    JPathSegment(..),
    -- * Building
    JPathBuilder,
    emptyJPathBuilder,
    addJPathSegment,
    buildJPath,
    -- * Rendering
    renderJPath,
  ) where

import Data.Text (Text)
import Data.Char (isAlpha, isAlphaNum)

import qualified Data.List as List
import qualified Data.Text as Text

-- | A single JSONPath segment.
data JPathSegment =
  JPSField Text | JPSIndex Int
  deriving (Eq, Ord, Show)

-- | A complete JSONPath.
newtype JPath = JPath [JPathSegment]
  deriving (Eq, Ord, Show)

-- | JSONPath segments in reverse order. Useful for fast construction of
-- paths.
newtype JPathBuilder = JPathBuilder [JPathSegment]
  deriving (Eq, Show)

emptyJPathBuilder :: JPathBuilder
emptyJPathBuilder = JPathBuilder []

-- | Append a path segment to the path.
addJPathSegment :: JPathSegment -> JPathBuilder -> JPathBuilder
addJPathSegment ps (JPathBuilder pss) = JPathBuilder (ps:pss)

buildJPath :: JPathBuilder -> JPath
buildJPath (JPathBuilder pss) = JPath (List.reverse pss)

renderJPath :: JPath -> Text
renderJPath (JPath ps) = "$" <> foldMap formatSegment ps
  where
    formatSegment :: JPathSegment -> Text
    formatSegment = \case
      JPSField key
        | isIdentifierKey key -> "." <> key
        | otherwise -> "['" <> escapeKey key <> "']"
      JPSIndex idx -> "[" <> Text.pack (show idx) <> "]"

    isIdentifierKey :: Text -> Bool
    isIdentifierKey s = case Text.uncons s of
      Nothing -> False
      Just (x, xs) -> isAlpha x && Text.all isAlphaNum xs

    escapeKey :: Text -> Text
    escapeKey = Text.concatMap escapeChar

    escapeChar :: Char -> Text
    escapeChar '\'' = "\\'"
    escapeChar '\\' = "\\\\"
    escapeChar c    = Text.singleton c
