{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JSONPath definition and rendering:
--
-- @
-- b :: 'JPathBuilder'
-- b = 'addJPathSegment' ('JPSIndex' 3) $
--     'addJPathSegment' ('JPSField' "fld") $
--     'addJPathSegment' ('JPSField' "dlf") $
--     'emptyJPathBuilder'
--
-- p :: 'JPath'
-- p = 'buildJPath' b
--
-- ghci> 'renderJPath' p
-- "$.dlf.fld[3]"
-- @
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
data JPathSegment
  = JPSField Text  -- ^ Record field name
  | JPSIndex Int   -- ^ Array element index
  deriving (Eq, Ord, Show)

-- | A complete JSONPath.
newtype JPath = JPath [JPathSegment]
  deriving (Eq, Ord, Show)

-- | JSONPath segments in reverse order. Useful for fast construction of paths
-- by appending path segments.
newtype JPathBuilder = JPathBuilder [JPathSegment]
  deriving (Eq, Show)

-- | A 'JPathBuilder' that does not contain any path segments.
emptyJPathBuilder :: JPathBuilder
emptyJPathBuilder = JPathBuilder []

-- | Append a path segment to the path.
addJPathSegment :: JPathSegment -> JPathBuilder -> JPathBuilder
addJPathSegment ps (JPathBuilder pss) = JPathBuilder (ps:pss)

-- | Finish building a JSONPath. \( O(n) \) in the amount of segments.
buildJPath :: JPathBuilder -> JPath
buildJPath (JPathBuilder pss) = JPath (List.reverse pss)

-- | Render a JSONPath as text.
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
