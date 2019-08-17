{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- A framework for building JSON schemas that can be used to perform both
-- validation and serialization.
module Jijo.Definition
  ( -- * Core
    JDefinition,
    jDefinition,
    validateViaDefinition,
    encodeViaDefinition,
    mapJError,
    -- ** Validation
    JTy(..),
    JValidationError(..),
    JValidationReport(..),
    isEmptyJValidationReport,
    addJValidationError,
    scopeJValidationReport,
    flattenJValidationReport,
    singletonJValidationReport,
    JValidation,
    jValidationError,
    jValidationFail,
    mapJValidationError,
    eitherToJValidation,
    -- ** Defining objects
    JObjectDefinition,
    jObjectDefinition,
    defineJObject,
    jField,
    jFieldOpt,
    inJField,
    inOptJField,
    -- ** Defining arrays
    jArrayOf,
    jListOf,
    -- ** Defining sums
    defineJSum,
    jEnumOption,
    jSumOption,
    JSumOption(..),
    JSumException(..),
    -- ** Stock definitions
    jObject,
    jArray,
    jString,
    jNumber,
    jBool,
    jNullable,
    -- ** Aeson integration
    parseJSON_viaDefinition,
    toJSON_viaDefinition,
    aesonJDefinition,
  ) where

import Prelude hiding ((.), id)
import Data.Text (Text)
import Data.DList (DList)
import Data.Scientific (Scientific)
import Data.Map (Map)
import Data.Set (Set)
import Data.Vector (Vector)
import GHC.TypeLits as TypeLits
import Control.Monad
import Control.Category
import Data.Coerce
import Data.Maybe
import Data.String
import Control.Exception (Exception, throw)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.DList as DList
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Aeson.Types as JSON

import qualified Control.Monad.Trans.Reader as T
import Data.Functor.Const
import Data.Functor.Product

import Data.Functor.Identity
import qualified Data.Monoid as Monoid
import Data.Profunctor
import Data.Tagged

import qualified GHC.Records as Rec

import Jijo.Path
import Jijo.Validation
import Jijo.RecordField

----------------------------------------------------------------------------
-- Definition
----------------------------------------------------------------------------

newtype EncodingArr j a =
  EncodingArr (a -> j)

instance Category EncodingArr where
  id = EncodingArr id
  EncodingArr f . EncodingArr g = EncodingArr (g . f)

data ArrPair p q j a = ArrPair (p j a) (q j a)

instance (Category p, Category q) => Category (ArrPair p q) where
  id = ArrPair id id
  ArrPair f1 g1 . ArrPair f2 g2 = ArrPair (f1 . f2) (g1 . g2)

-- | Describes serialization and validation of a type @a@ with possible
-- validation errors of type @e@.
type JDefinition e = ArrPair (ValidationArr e) EncodingArr

jValidate :: JDefinition e j a -> j -> JValidation e a
jValidate (ArrPair (ValidationArr vArr) _) = vArr

jEncode :: JDefinition e j a -> a -> j
jEncode (ArrPair _ (EncodingArr eArr)) = eArr

validateViaDefinition ::
  JDefinition e j a ->
  j ->
  Either (JValidationReport e) a
validateViaDefinition d j =
  case jValidate d j of
    JValidation (Just a) es | isEmptyJValidationReport es -> Right a
    JValidation _ es -> Left es

encodeViaDefinition ::
  JDefinition e j a ->
  a ->
  j
encodeViaDefinition = jEncode

-- | Define @b@ in terms of @a@.
jDefinition ::
  (a -> JValidation e b) ->
  (b -> a) ->
  JDefinition e a b
jDefinition toB fromB = ArrPair (ValidationArr toB) (EncodingArr fromB)

mapJError :: (e -> e') -> JDefinition e a b -> JDefinition e' a b
mapJError f (ArrPair (ValidationArr toB) fromB) =
  ArrPair (ValidationArr (mapJValidationError f . toB)) fromB

----------------------------------------------------------------------------
-- Objects
----------------------------------------------------------------------------

type ObjValidationAp e = T.ReaderT JSON.Object (JValidation e)
type ObjEncodingAp o = Const (o -> DList JSON.Pair)
type ObjDefinitionAp e o = Product (ObjValidationAp e) (ObjEncodingAp o)

-- | Auxiliary type for describing objects, suitable for use with
-- applicative notation. Usually it will be immediately fed into
-- 'defineJObject'.
--
--   * @e@ is the error type
--   * @o@ is the object type being described
--   * @a@ is the return type; a completed description will have type
--     @JObjectDefinition e o o@.
newtype JObjectDefinition e o a =
  JObjectDefinition (ObjDefinitionAp e o a)
  deriving newtype (Functor, Applicative)

jObjectValidate :: JObjectDefinition e o a -> JSON.Object -> JValidation e a
jObjectValidate (JObjectDefinition (Pair (T.ReaderT vAp) _)) = vAp

jObjectEncode :: JObjectDefinition e o a -> o -> DList JSON.Pair
jObjectEncode (JObjectDefinition (Pair _ (Const eAp))) = eAp

mkJObjectDefinition ::
  (JSON.Object -> JValidation e a) ->
  (o -> DList JSON.Pair) ->
  JObjectDefinition e o a
mkJObjectDefinition objValidate objEncode =
  JObjectDefinition (Pair (T.ReaderT objValidate) (Const objEncode))

type MonadicObjectErr =
  'TypeLits.Text "Monadic object definition is not supported. Fit your definition into" ':$$:
  'TypeLits.Text "  -XApplicativeDo if possible, or use 'Category' composition" ':$$:
  'TypeLits.Text "  if you need to do more checks after the initial validation."

instance TypeError MonadicObjectErr => Monad (JObjectDefinition e o) where
  return = error "return @ JObjectDefinition: impossible"
  (>>=) = error "(>>=) @ JObjectDefinition: impossible"

jObjectDefinition :: JObjectDefinition e o o -> JDefinition e JSON.Object o
jObjectDefinition objDefn = jDefinition validationArr encodingArr
  where
    validationArr = jObjectValidate objDefn
    encodingArr = HashMap.fromList . DList.toList . jObjectEncode objDefn

defineJObject :: JObjectDefinition e o o -> JDefinition e JSON.Value o
defineJObject objDefn = jObjectDefinition objDefn . jObject

inJField :: Text -> (o -> a) -> JDefinition e JSON.Value a -> JObjectDefinition e o a
inJField fieldName getField fieldDef = mkJObjectDefinition objValidate objEncode
  where
    objValidate = jValidateField fieldName (jValidate fieldDef)
    objEncode o = DList.singleton (fieldName, jEncode fieldDef (getField o))

inOptJField :: Text -> (o -> Maybe a) -> JDefinition e JSON.Value a -> JObjectDefinition e o (Maybe a)
inOptJField fieldName getField fieldDef = mkJObjectDefinition objValidate objEncode
  where
    objValidate = jValidateOptField fieldName (jValidate fieldDef)
    objEncode o = case getField o of
      Nothing -> DList.empty
      Just val -> DList.singleton (fieldName, jEncode fieldDef val)

jField ::
  forall name prefix e o a.
  Rec.HasField (AppendSymbol prefix name) o a =>
  Text ->
  JDefinition e JSON.Value a ->
  JObjectDefinition e o (Field prefix name a)
jField fieldName fieldDef =
  coerceJObjectDefinition $ inJField fieldName
    (Rec.getField @(AppendSymbol prefix name) @o @a)
    fieldDef

jFieldOpt ::
  forall name prefix e o a.
  Rec.HasField (AppendSymbol prefix name) o (Maybe a) =>
  Text ->
  JDefinition e JSON.Value a ->
  JObjectDefinition e o (Field prefix name (Maybe a))
jFieldOpt fieldName fieldDef =
  coerceJObjectDefinition $ inOptJField fieldName
    (Rec.getField @(AppendSymbol prefix name) @o @(Maybe a))
    fieldDef

coerceJObjectDefinition ::
  Coercible a b =>
  JObjectDefinition e o a ->
  JObjectDefinition e o b
coerceJObjectDefinition (JObjectDefinition (Pair p q)) =
  JObjectDefinition (Pair (coerce p) (coerce q))

----------------------------------------------------------------------------
-- Arrays
----------------------------------------------------------------------------

jArrayOf :: JDefinition e JSON.Value a -> JDefinition e JSON.Value (Vector a)
jArrayOf elementDefn = jDefinition validationArr encodingArr . jArray
  where
    validationArr = jValidateElements (jValidate elementDefn)
    encodingArr = Vector.map (jEncode elementDefn)

jListOf :: JDefinition e JSON.Value a -> JDefinition e JSON.Value [a]
jListOf elementDefn =
  jDefinition (pure . Vector.toList) Vector.fromList . jArrayOf elementDefn

----------------------------------------------------------------------------
-- Sums
----------------------------------------------------------------------------

data JSumOption e a
  = JEnumOption a (a -> Bool)
  | forall b. JSumOption (b -> a) (a -> Maybe b) (JDefinition e JSON.Value b)

data JSumException
  = JSumNoEncoding
  | JSumAmbiguousEncoding
  deriving (Eq, Show)

instance Exception JSumException

jEnumOption :: Text -> Prism' a () -> Map Text (JSumOption e a)
jEnumOption label p =
  Map.singleton label (JEnumOption (review p ()) (isJust . preview p))

jSumOption :: Text -> Prism' a b -> JDefinition e JSON.Value b -> Map Text (JSumOption e a)
jSumOption label p jDef =
  Map.singleton label (JSumOption (review p) (preview p) jDef)

defineJSum :: Map Text (JSumOption e a) -> JDefinition e JSON.Value a
defineJSum jSumOptions = jDefinition checkSum encodeSum
  where
    checkSum = \case
      JSON.String label ->
        lookupLabel label $ \case
          JEnumOption a _ -> pure a
          JSumOption{} -> jValidationError JMalformedSum
      JSON.Object obj ->
        case HashMap.toList obj of
          [(label, j)] ->
            lookupLabel label $ \case
              JEnumOption{} -> jValidationError JMalformedSum
              JSumOption f _ jDef ->
                mapJValidationReport (scopeJValidationReport (JPSField label)) $
                f <$> jValidate jDef j
          _ -> jValidationError JMalformedSum
      _ -> jValidationError (JTypeNotOneOf allowedTypes)

    encodeSum a = pickEncoding $ do
      (label, jOpt) <- Map.toList jSumOptions
      case jOpt of
        JEnumOption _ match -> do
          guard (match a)
          [JSON.String label]
        JSumOption _ match jDef -> do
          b <- maybeToList (match a)
          [JSON.object [label JSON..= jEncode jDef b]]

    pickEncoding [enc] = enc
    pickEncoding [] = throw JSumNoEncoding
    pickEncoding _ = throw JSumAmbiguousEncoding

    allowedTypes =
      Set.fromList [ JTyString | JEnumOption{} <- Map.elems jSumOptions] <>
      Set.fromList [ JTyObject | JSumOption{}  <- Map.elems jSumOptions]

    lookupLabel s cont =
      case Map.lookup s jSumOptions of
        Nothing -> jValidationError (JLabelNotOneOf (Map.keysSet jSumOptions))
        Just opt -> cont opt

----------------------------------------------------------------------------
-- Stock definitions
----------------------------------------------------------------------------

jObject :: JDefinition e JSON.Value JSON.Object
jObject = jDefinition checkObject JSON.Object
  where
    checkObject = \case
      JSON.Object o -> pure o
      _ -> jValidationError (JTypeNotOneOf (Set.singleton JTyObject))

jArray :: JDefinition e JSON.Value JSON.Array
jArray = jDefinition checkArray JSON.Array
  where
    checkArray = \case
      JSON.Array a -> pure a
      _ -> jValidationError (JTypeNotOneOf (Set.singleton JTyArray))

jString :: JDefinition e JSON.Value Text
jString = jDefinition checkString JSON.String
  where
    checkString = \case
      JSON.String s -> pure s
      _ -> jValidationError (JTypeNotOneOf (Set.singleton JTyString))

jNumber :: JDefinition e JSON.Value Scientific
jNumber = jDefinition checkNumber JSON.Number
  where
    checkNumber = \case
      JSON.Number n -> pure n
      _ -> jValidationError (JTypeNotOneOf (Set.singleton JTyNumber))

jBool :: JDefinition e JSON.Value Bool
jBool = jDefinition checkBool JSON.Bool
  where
    checkBool = \case
      JSON.Bool b -> pure b
      _ -> jValidationError (JTypeNotOneOf (Set.singleton JTyBool))

-- | 'jNullable' assumses that the input definition is disjoint with 'null'. As
-- a consequence, @jNullable (jNullable ...)@ is a programmer mistake.
jNullable :: JDefinition e JSON.Value a -> JDefinition e JSON.Value (Maybe a)
jNullable jDef = jDefinition validateNullable encodeNullable
  where
    -- Assumption: for any a, jEncode jDef a /= JSON.Null
    encodeNullable = \case
      Nothing -> JSON.Null
      Just a -> jEncode jDef a

    -- Assumption: (jValidate jDef JSON.Null) errors with JTypeNotOneOf
    validateNullable JSON.Null = pure Nothing
    validateNullable j =
      mapJValidationReport adjustReport (fmap Just (jValidate jDef j))

    adjustReport (JValidationReport es fs) =
      JValidationReport (map adjustErr es) fs
    adjustErr (JTypeNotOneOf jtys) =
      JTypeNotOneOf (Set.insert JTyNull jtys)
    adjustErr e = e

----------------------------------------------------------------------------
-- Aeson integration
----------------------------------------------------------------------------

-- | Default definition for 'JSON.FromJSON':
-- @
-- instance FromJSON Foo where
--   parseJSON = parseJSON_viaDefinition jFoo
-- @
parseJSON_viaDefinition ::
  Show e =>
  JDefinition e JSON.Value a ->
  JSON.Value -> JSON.Parser a
parseJSON_viaDefinition d j =
  either (fail . renderJValidationErrorList . flattenJValidationReport) return $
  validateViaDefinition d j

renderJValidationErrorList ::
  forall e. Show e =>
  [(JPath, JValidationError e)] ->
  String
renderJValidationErrorList =
  mconcat . List.intersperse "\n" . map formatError
  where
    formatError :: (JPath, JValidationError e) -> String
    formatError (path, err) =
      (fromString . Text.unpack) (renderJPath path) <> ": " <>
      case err of
        JTypeNotOneOf jtys -> "type not one of " <> pprSet pprJTy jtys
        JLabelNotOneOf jlabels -> "label not one of " <> pprSet (fromString . Text.unpack) jlabels
        JMissingField fname -> "missing field " <> (fromString . Text.unpack) fname
        JMalformedSum -> "malformed sum"
        JValidationFail e -> fromString (show e)
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

-- | Default definition for 'JSON.ToJSON':
-- @
-- instance ToJSON Foo where
--   toJSON = toJSON_viaDefinition jFoo
-- @
toJSON_viaDefinition ::
  JDefinition e JSON.Value a ->
  a -> JSON.Value
toJSON_viaDefinition = jEncode

aesonJDefinition ::
  (JSON.FromJSON a, JSON.ToJSON a) =>
  JDefinition String JSON.Value a
aesonJDefinition = jDefinition toB fromB
  where
    toB j =
      either jValidationFail pure $
      JSON.parseEither JSON.parseJSON j
    fromB = JSON.toJSON

----------------------------------------------------------------------------
-- Prisms (to avoid a 'lens' dep)
----------------------------------------------------------------------------

type Prism s t a b = forall p f. (Choice p, Applicative f) => p a (f b) -> p s (f t)
type Prism' s a = Prism s s a a

review :: Prism' t b -> b -> t
review p = coerce . p . Tagged . Identity
{-# INLINE review #-}

preview :: Prism' s a -> s -> Maybe a
preview l = coerce . l (Const . Monoid.First . Just)
{-# INLINE preview #-}
