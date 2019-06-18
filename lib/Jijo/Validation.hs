{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Jijo.Validation
  ( JTy(..),
    JValidationError(..),
    JValidation(..),
    jValidationError,
    jValidationFail,
    jValidationCompose,
    jValidationLocal,
    jValidateField,
    jValidateOptField,
  ) where

import Prelude hiding ((.), id)
import Data.Text (Text)
import Data.Set (Set)
import Control.Category
import Control.Monad
import Data.Coerce
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Traversable

import GHC.TypeLits hiding (ErrorMessage(Text))
import qualified GHC.TypeLits as TypeLits

import qualified Data.HashMap.Strict as HashMap

import qualified Control.Monad.Trans.Reader as T
import qualified Control.Monad.Trans.Maybe as T
import qualified Control.Monad.Trans.State as T

import Jijo.Path

-- | Set of possible JSON types.
data JTy
  = JTyObject
  | JTyArray
  | JTyString
  | JTyNumber
  | JTyBool
  | JTyNull
  deriving (Eq, Ord, Show)

data JValidationError e
  = JTypeNotOneOf (Set JTy)
  | JLabelNotOneOf (Set Text)
  | JMissingField Text
  | JMalformedSum
  | JValidationFail e
  deriving (Eq, Show)

type ErrorList e = [(JPath, JValidationError e)]

-- | Validation applicative that grants:
--
-- * Access to the current JSON path.
-- * Accumulation of error messages.
-- * Possibility of failure.
newtype JValidation e a =
  JValidation { runJValidation :: JPathBuilder -> ErrorList e -> (Maybe a, ErrorList e) }
  deriving stock Functor

-- JValidation is a composition of applicative functors.
-- We derive the 'Applicative' instance via this representation.
type JValidationA e =
  T.Reader JPathBuilder `Compose`
  T.State (ErrorList e) `Compose`
  Maybe

-- JValidation is a composition of monad transformers.
-- We derive the Kleisli composition via this representation.
type JValidationM e =
  T.ReaderT JPathBuilder
    (T.MaybeT
      (T.State (ErrorList e)))

type PureSig f a = a -> f a
type ApSig f a b = f (a -> b) -> f a -> f b

type CoercePureSig f f' =
  forall a.
  PureSig f  a ->
  PureSig f' a

type CoerceApSig f f' =
  forall a b.
  ApSig f  a b ->
  ApSig f' a b

instance Applicative (JValidation e) where
  pure = (coerce :: CoercePureSig (JValidationA e) (JValidation e)) pure
  (<*>) = (coerce :: CoerceApSig (JValidationA e) (JValidation e)) (<*>)

type MonadicValidationErrorMessage =
  'TypeLits.Text "Monadic validation is not supported. Fit your definition into" ':$$:
  'TypeLits.Text "  -XApplicativeDo if possible, or use 'Category' composition" ':$$:
  'TypeLits.Text "  if you need to do more checks after the initial validation."

instance TypeError MonadicValidationErrorMessage => Monad (JValidation e) where
  return = error "return @JValidation: impossible"
  (>>=) = error "(>>=) @JValidation: impossible"

type KleisliCompositionSig m a b c = (b -> m c) -> (a -> m b) -> (a -> m c)
type CoerceKleisliCompositionSig m m' =
  forall a b c.
  KleisliCompositionSig m  a b c ->
  KleisliCompositionSig m' a b c

jValidationCompose :: KleisliCompositionSig (JValidation e) a b c
jValidationCompose =
  (coerce :: CoerceKleisliCompositionSig (JValidationM e) (JValidation e)) (<=<)

jValidationError :: JValidationError e -> JValidation e a
jValidationError e =
  JValidation $ \pb es ->
    (Nothing, (buildJPath pb, e):es)

jValidationFail :: e -> JValidation e a
jValidationFail = jValidationError . JValidationFail

type LocalSig m r a = (r -> r) -> m a -> m a

type CoerceLocalSig m m' =
  forall r a.
  LocalSig m  r a ->
  LocalSig m' r a

jValidationLocal ::
  (JPathBuilder -> JPathBuilder) ->
  JValidation e a -> JValidation e a
jValidationLocal =
  (coerce :: CoerceLocalSig (JValidationM e) (JValidation e)) T.local

jValidateField ::
  Text ->
  (j -> JValidation e a) ->
  HashMap.HashMap Text j ->
  JValidation e a
jValidateField fieldName vField o =
  onJust (HashMap.lookup fieldName o) $ \field ->
    jValidationLocal (addJPathSegment (JPSField fieldName)) $
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
    jValidationLocal (addJPathSegment (JPSField fieldName)) $
    vField field
