{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}

-- | Validation of JSON input against a schema.
module Jijo.Validation
  ( JTy(..),
    JValidationError(..),
    JValidationReport(..),
    isEmptyJValidationReport,
    scopeJValidationReport,
    flattenJValidationReport,
    renderJValidationReport,
    renderJValidationErrorList,
    JValidation(..),
    jValidationWarning,
    jValidationError,
    jValidationFail,
    jRejectExtraFields,
    jValidateField,
    jValidateOptField,
    jValidateElements,
    mapJValidationError,
    mapJValidationReport,
    eitherToJValidation,
    ValidationArr(..),
  ) where

import Prelude hiding ((.), id)
import Data.Text (Text)
import Data.Set (Set)
import Data.HashSet (HashSet)
import Data.Map (Map)
import Control.Category
import Data.Foldable
import Data.Traversable
import Data.Bifunctor
import Data.String
import Data.Functor.Compose
import qualified Control.Monad.Trans.State.Strict as State.Strict

import GHC.TypeLits hiding (ErrorMessage(Text))
import qualified GHC.TypeLits as TypeLits

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Data.Text as Text

import Jijo.Path

-- | JSON value types.
data JTy
  = JTyObject
  | JTyArray
  | JTyString
  | JTyNumber
  | JTyBool
  | JTyNull
  deriving stock (Eq, Ord, Show)

-- | JSON validation error, parametrized by @e@ for domain-specific validation
-- failures.
--
-- @
-- data DomainError = BadUserId | PasswordTooShort
-- type ValidationError = 'JValidationError' DomainError
-- @
data JValidationError e
  = JTypeNotOneOf (Set JTy)   -- ^ A value does not have the expected type, e.g. expected an object or null but got a string.
  | JLabelNotOneOf (Set Text) -- ^ A value of the form @{ \"label\": value }@ or @\"label\"@ has an unexpected label,
                              --   e.g. expected @\"just\"@ or @\"nothing\"@ but got @\"candy\"@.
  | JMissingField Text        -- ^ An object does not have a required field.
  | JExtraField Text          -- ^ An object has an unexpected field.
  | JMalformedSum             -- ^ A value is not a valid encoding of a sum type,
                              --   which should have the form @{ \"label\": value }@ or @\"label\"@.
  | JValidationFail e         -- ^ A domain-specific validation failure.
  deriving stock (Eq, Show)
  deriving stock Functor

instance IsString e => IsString (JValidationError e) where
  fromString = JValidationFail . fromString

-- | A collection of 'JValidationError', both for the root JSON value and its
-- children (object fields or array elements). In other words,
-- @'JValidationReport' e@ is a prefix tree where the keys are 'JPathSegment'
-- and the values are @['JValidationError' e]@.
data JValidationReport e =
  JValidationReport [JValidationError e] (Map JPathSegment (JValidationReport e))
  deriving stock (Eq, Show)
  deriving stock Functor

instance Semigroup (JValidationReport e) where
  JValidationReport es1 fs1 <> JValidationReport es2 fs2 =
    JValidationReport (es1 <> es2) (Map.unionWith (<>) fs1 fs2)

instance Monoid (JValidationReport e) where
  mempty = JValidationReport mempty mempty

-- | Associate a validation report with a child (object field or array element)
-- instead of the root value.
scopeJValidationReport :: JPathSegment -> JValidationReport e -> JValidationReport e
scopeJValidationReport ps es =
  JValidationReport [] (Map.singleton ps es)

-- | Check if validation was successful.
isEmptyJValidationReport :: JValidationReport e -> Bool
isEmptyJValidationReport (JValidationReport es fs) =
  null es && all isEmptyJValidationReport fs

-- | Flatten the prefix trie of 'JValidationError' into a list.
flattenJValidationReport :: JValidationReport e -> [(JPath, JValidationError e)]
flattenJValidationReport = go emptyJPathBuilder
  where
    go pb (JValidationReport es fs) =
      map (\e -> (buildJPath pb, e)) es <>
      concatMap (goField pb) (Map.toList fs)
    goField pb (ps, a) =
      go (addJPathSegment ps pb) a

-- | Validation context with an 'Applicative' instance that grants:
--
-- * Accumulation of errors in a validation report (@Writer@).
-- * Possibility of failure (@Maybe@).
--
data JValidation e a =
  JValidation (Maybe a) (JValidationReport e)
  deriving stock Functor

instance Applicative (JValidation e) where
  pure a = JValidation (pure a) mempty
  JValidation mf es1 <*> JValidation ma es2 =
    JValidation (mf <*> ma) (es1 <> es2)

type MonadicValidationErrorMessage =
  'TypeLits.Text "Monadic validation is not supported. Fit your definition into" ':$$:
  'TypeLits.Text "  -XApplicativeDo if possible, or use 'Category' composition" ':$$:
  'TypeLits.Text "  if you need to do more checks after the initial validation."

instance TypeError MonadicValidationErrorMessage => Monad (JValidation e) where
  return = error "return @JValidation: impossible"
  (>>=) = error "(>>=) @JValidation: impossible"

-- | A validation arrow with a 'Category' instance. This is a Kleisli arrow
-- associated with 'JValidation'.
newtype ValidationArr e j a =
  ValidationArr (j -> JValidation e a)

instance Category (ValidationArr e) where
  id = ValidationArr pure
  ValidationArr f . ValidationArr g =
    ValidationArr $ \a ->
      case g a of
        JValidation Nothing  es1 -> JValidation Nothing es1
        JValidation (Just b) es1 ->
          case f b of
            JValidation mc es2 -> JValidation mc (es1 <> es2)

-- | Report a warning by appending an error to the root of the @JValidationReport@
-- but without aborting validation.
jValidationWarning :: JValidationError e -> JValidation e ()
jValidationWarning e =
  JValidation (Just ()) (JValidationReport [e] mempty)

-- | Report an error by appending an error to the root of the @JValidationReport@
-- and aborting validation.
jValidationError :: JValidationError e -> JValidation e a
jValidationError e =
  JValidation Nothing (JValidationReport [e] mempty)

-- | A variant of 'jValidationError' specialized to domain-specific errors.
jValidationFail :: e -> JValidation e a
jValidationFail = jValidationError . JValidationFail

-- | A natural transformation between 'Either' and 'JValidation'.
eitherToJValidation :: Either e a -> JValidation e a
eitherToJValidation = either jValidationFail pure

-- | Modify the validation report using produced in 'JValidation'.
mapJValidationReport ::
  (JValidationReport e -> JValidationReport e') ->
  JValidation e a -> JValidation e' a
mapJValidationReport f (JValidation a es) =
  JValidation a (f es)

-- | Modify domain-specific validation errors produced in 'JValidation'.
mapJValidationError :: (e -> e') -> JValidation e a -> JValidation e' a
mapJValidationError f = mapJValidationReport (fmap f)

instance Bifunctor JValidation where
  first = mapJValidationError
  second = fmap

-- | Report a validation warning if a JSON object contains unexpected fields.
jRejectExtraFields ::
  HashSet Text ->
  HashMap.HashMap Text j ->
  JValidation e ()
jRejectExtraFields allowedFields obj =
  traverse_
    (\fname -> jValidationWarning (JExtraField fname))
    (HashSet.toList extraFields)
  where
    objFields = HashMap.keysSet obj
    extraFields = HashSet.difference objFields allowedFields

-- | Validate a required object field.
jValidateField ::
  Text ->
  (j -> JValidation e a) ->
  HashMap.HashMap Text j ->
  JValidation e a
jValidateField fieldName vField o =
  onJust (HashMap.lookup fieldName o) $ \field ->
    mapJValidationReport (scopeJValidationReport (JPSField fieldName)) $
    vField field
  where
    onJust Nothing _ = jValidationError (JMissingField fieldName)
    onJust (Just a) f = f a

-- | Validate an optional object field.
jValidateOptField ::
  Text ->
  (j -> JValidation e a) ->
  HashMap.HashMap Text j ->
  JValidation e (Maybe a)
jValidateOptField fieldName vField o =
  for (HashMap.lookup fieldName o) $ \field ->
    mapJValidationReport (scopeJValidationReport (JPSField fieldName)) $
    vField field

-- | Validate array elements.
jValidateElements ::
  (j -> JValidation e a) ->
  Vector.Vector j ->
  JValidation e (Vector.Vector a)
jValidateElements vElement =
  itraverse $ \i element ->
    mapJValidationReport (scopeJValidationReport (JPSIndex i)) $
    vElement element


-- | Render a @JValidationReport@ into a human-readable string.
renderJValidationReport :: JValidationReport String -> String
renderJValidationReport = renderJValidationErrorList . flattenJValidationReport

-- | Render a flattened @JValidationReport@ into a human-readable string.
renderJValidationErrorList ::
  [(JPath, JValidationError String)] ->
  String
renderJValidationErrorList =
  mconcat . List.intersperse "\n" . map formatError
  where
    formatError :: (JPath, JValidationError String) -> String
    formatError (path, err) =
      (fromString . Text.unpack) (renderJPath path) <> ": " <>
      case err of
        JTypeNotOneOf jtys -> "type not one of " <> pprSet pprJTy jtys
        JLabelNotOneOf jlabels -> "label not one of " <> pprSet (fromString . Text.unpack) jlabels
        JMissingField fname -> "missing field " <> (fromString . Text.unpack) fname
        JExtraField fname -> "extra field " <> (fromString . Text.unpack) fname
        JMalformedSum -> "malformed sum"
        JValidationFail e -> fromString e
    pprSet :: (a -> String) -> Set a -> String
    pprSet pprElem s =
      "{" <> (mconcat . List.intersperse ",") (map pprElem (Set.toList s)) <> "}"
    pprJTy :: JTy -> String
    pprJTy = \case
      JTyObject -> "object"
      JTyArray -> "array"
      JTyString -> "string"
      JTyNumber -> "number"
      JTyBool -> "bool"
      JTyNull -> "null"

-- Indexed 'traverse' using 'State'.
-- Does not require 'Monad' unlike 'Vector.imapM'.
itraverse :: (Traversable t, Applicative f) => (Int -> a -> f b) -> t a -> f (t b)
itraverse f s = State.Strict.evalState (getCompose (traverse f' s)) 0
  where f' a = Compose (State.Strict.state (\i -> i `seq` (f i a, i+1)))
