{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}

module Jijo.Validation
  ( JTy(..),
    JValidationError(..),
    JValidationReport(..),
    isEmptyJValidationReport,
    addJValidationError,
    scopeJValidationReport,
    flattenJValidationReport,
    singletonJValidationReport,
    JValidation(..),
    jValidationError,
    jValidationFail,
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
import Data.Map (Map)
import Control.Category
import Data.Traversable
import Data.String
import Data.Functor.Compose
import qualified Control.Monad.Trans.State.Strict as State.Strict

import GHC.TypeLits hiding (ErrorMessage(Text))
import qualified GHC.TypeLits as TypeLits

import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

import Jijo.Path

-- | Set of possible JSON types.
data JTy
  = JTyObject
  | JTyArray
  | JTyString
  | JTyNumber
  | JTyBool
  | JTyNull
  deriving stock (Eq, Ord, Show)

data JValidationError e
  = JTypeNotOneOf (Set JTy)
  | JLabelNotOneOf (Set Text)
  | JMissingField Text
  | JMalformedSum
  | JValidationFail e
  deriving stock (Eq, Show)
  deriving stock Functor

instance IsString e => IsString (JValidationError e) where
  fromString = JValidationFail . fromString

data JValidationReport e =
  JValidationReport [JValidationError e] (Map JPathSegment (JValidationReport e))
  deriving stock (Eq, Show)
  deriving stock Functor

instance Semigroup (JValidationReport e) where
  JValidationReport es1 fs1 <> JValidationReport es2 fs2 =
    JValidationReport (es1 <> es2) (Map.unionWith (<>) fs1 fs2)

instance Monoid (JValidationReport e) where
  mempty = JValidationReport mempty mempty

addJValidationError :: JValidationError e -> JValidationReport e -> JValidationReport e
addJValidationError e (JValidationReport es fs) = JValidationReport (e:es) fs

scopeJValidationReport :: JPathSegment -> JValidationReport e -> JValidationReport e
scopeJValidationReport ps es =
  JValidationReport [] (Map.singleton ps es)

isEmptyJValidationReport :: JValidationReport e -> Bool
isEmptyJValidationReport (JValidationReport es fs) =
  null es && all isEmptyJValidationReport fs

flattenJValidationReport :: JValidationReport e -> [(JPath, JValidationError e)]
flattenJValidationReport = go emptyJPathBuilder
  where
    go pb (JValidationReport es fs) =
      map (\e -> (buildJPath pb, e)) es <>
      concatMap (goField pb) (Map.toList fs)
    goField pb (ps, a) =
      go (addJPathSegment ps pb) a

singletonJValidationReport :: JValidationError e -> JValidationReport e
singletonJValidationReport e = JValidationReport [e] Map.empty

-- | Validation applicative that grants:
--
-- * Accumulation of a validation report.
-- * Possibility of failure.
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

jValidationError :: JValidationError e -> JValidation e a
jValidationError e =
  JValidation Nothing (JValidationReport [e] mempty)

jValidationFail :: e -> JValidation e a
jValidationFail = jValidationError . JValidationFail

eitherToJValidation :: Either e a -> JValidation e a
eitherToJValidation = either jValidationFail pure

mapJValidationReport ::
  (JValidationReport e -> JValidationReport e') ->
  JValidation e a -> JValidation e' a
mapJValidationReport f (JValidation a es) =
  JValidation a (f es)

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

jValidateOptField ::
  Text ->
  (j -> JValidation e a) ->
  HashMap.HashMap Text j ->
  JValidation e (Maybe a)
jValidateOptField fieldName vField o =
  for (HashMap.lookup fieldName o) $ \field ->
    mapJValidationReport (scopeJValidationReport (JPSField fieldName)) $
    vField field

jValidateElements ::
  (j -> JValidation e a) ->
  Vector.Vector j ->
  JValidation e (Vector.Vector a)
jValidateElements vElement =
  itraverse $ \i element ->
    mapJValidationReport (scopeJValidationReport (JPSIndex i)) $
    vElement element

mapJValidationError :: (e -> e') -> JValidation e a -> JValidation e' a
mapJValidationError f = mapJValidationReport (fmap f)

-- Indexed 'traverse' using 'State'.
-- Does not require 'Monad' unlike 'Vector.imapM'.
itraverse :: (Traversable t, Applicative f) => (Int -> a -> f b) -> t a -> f (t b)
itraverse f s = State.Strict.evalState (getCompose (traverse f' s)) 0
  where f' a = Compose (State.Strict.state (\i -> i `seq` (f i a, i+1)))
