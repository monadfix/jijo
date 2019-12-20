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

-- | A framework for building JSON schemas that can be used to perform both
-- validation and serialization.
module Jijo.Definition
  ( -- * Core
    JDefinition,
    jDefinition,
    validateViaDefinition,
    encodeViaDefinition,
    mapJError,
    EncodingArr(..),
    ArrPair(..),
    -- * Validation
    JTy(..),
    JValidationError(..),
    JValidationReport(..),
    isEmptyJValidationReport,
    scopeJValidationReport,
    flattenJValidationReport,
    renderJValidationReport,
    renderJValidationErrorList,
    JValidation,
    jValidationError,
    jValidationFail,
    mapJValidationError,
    eitherToJValidation,
    -- * Defining objects
    JObjectDefinition,
    jObjectDefinition,
    jObjectDefinitionEither,
    defineJObject,
    defineJObjectEither,
    allowExtraFields,
    jField,
    jFieldOpt,
    inJField,
    inOptJField,
    -- * Defining arrays
    jArrayOf,
    jListOf,
    -- * Defining sums
    defineJSum,
    jEnumOption,
    jSumOption,
    JSumOption(..),
    JSumException(..),
    -- * Stock definitions
    jObject,
    jArray,
    jString,
    jNumber,
    jBool,
    jNullable,
    -- * Aeson integration
    parseJSON_viaDefinition,
    toJSON_viaDefinition,
    aesonJDefinition,
  ) where

import Prelude hiding ((.), id)
import Data.Text (Text)
import Data.DList (DList)
import Data.Scientific (Scientific)
import Data.Map (Map)
import Data.HashSet (HashSet)
import Data.Vector (Vector)
import GHC.TypeLits as TypeLits
import Control.Monad
import Control.Category
import Data.Coerce
import Data.Maybe
import Control.Exception (Exception, throw)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.DList as DList
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
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

-- | Encode @a@ as @j@.
newtype EncodingArr j a =
  EncodingArr (a -> j)

instance Category EncodingArr where
  id = EncodingArr id
  EncodingArr f . EncodingArr g = EncodingArr (g . f)

-- | A pair of arrows, i.e. the product of two categories.
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

-- | Validate @j@ against a schema, producing either a validation report (in
-- case of failure) or a value of type @a@ (in case of success).
validateViaDefinition ::
  JDefinition e j a ->
  j ->
  Either (JValidationReport e) a
validateViaDefinition d j =
  case jValidate d j of
    JValidation (Just a) es | isEmptyJValidationReport es -> Right a
    JValidation _ es -> Left es

-- | Serialize @a@ into @j@.
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

-- | Modify domain-specific validation errors produced in 'JDefinition'.
mapJError :: (e -> e') -> JDefinition e a b -> JDefinition e' a b
mapJError f (ArrPair (ValidationArr toB) fromB) =
  ArrPair (ValidationArr (mapJValidationError f . toB)) fromB

----------------------------------------------------------------------------
-- Objects
----------------------------------------------------------------------------

data ObjSchema =
  ObjSchema
    { objSchemaFields :: HashSet Text,
      objSchemaAllowExtraFields :: Bool
    }

instance Semigroup ObjSchema where
  s1 <> s2 =
    ObjSchema
      { objSchemaFields = HashSet.union (objSchemaFields s1) (objSchemaFields s2),
        objSchemaAllowExtraFields = objSchemaAllowExtraFields s1 || objSchemaAllowExtraFields s2
      }

instance Monoid ObjSchema where
  mempty =
    ObjSchema
      { objSchemaFields = HashSet.empty,
        objSchemaAllowExtraFields = False
      }

type ObjValidationAp e = T.ReaderT JSON.Object (JValidation e)
type ObjEncodingAp o = Const (o -> DList JSON.Pair)
type ObjSchemaAp = Const ObjSchema
type ObjDefinitionAp e o = Product (Product ObjSchemaAp (ObjValidationAp e)) (ObjEncodingAp o)

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
jObjectValidate (JObjectDefinition (Pair (Pair (Const objSchema) (T.ReaderT vAp)) _)) obj =
  unless
    (objSchemaAllowExtraFields objSchema)
    (jRejectExtraFields (objSchemaFields objSchema) obj) *>
  vAp obj

jObjectEncode :: JObjectDefinition e o a -> o -> DList JSON.Pair
jObjectEncode (JObjectDefinition (Pair _ (Const eAp))) = eAp

mkJObjectDefinition ::
  ObjSchema ->
  (JSON.Object -> JValidation e a) ->
  (o -> DList JSON.Pair) ->
  JObjectDefinition e o a
mkJObjectDefinition objSchema objValidate objEncode =
  JObjectDefinition (Pair (Pair (Const objSchema) (T.ReaderT objValidate)) (Const objEncode))

type MonadicObjectErr =
  'TypeLits.Text "Monadic object definition is not supported. Fit your definition into" ':$$:
  'TypeLits.Text "  -XApplicativeDo if possible, or use 'Category' composition" ':$$:
  'TypeLits.Text "  if you need to do more checks after the initial validation."

instance TypeError MonadicObjectErr => Monad (JObjectDefinition e o) where
  return = error "return @ JObjectDefinition: impossible"
  (>>=) = error "(>>=) @ JObjectDefinition: impossible"

-- | Construct 'JDefinition' from 'JObjectDefinition'.
jObjectDefinition :: JObjectDefinition e o o -> JDefinition e JSON.Object o
jObjectDefinition objDefn = jDefinition validationArr encodingArr
  where
    validationArr = jObjectValidate objDefn
    encodingArr = HashMap.fromList . DList.toList . jObjectEncode objDefn

-- | Construct 'JDefinition' from 'JObjectDefinition' with an additional validation step.
jObjectDefinitionEither :: JObjectDefinition e o (Either e o) -> JDefinition e JSON.Object o
jObjectDefinitionEither objDefn = ArrPair validationArr encodingArr
  where
    validationArr = ValidationArr eitherToJValidation . ValidationArr (jObjectValidate objDefn)
    encodingArr = EncodingArr (HashMap.fromList . DList.toList . jObjectEncode objDefn)

-- | Disable warnings when the JSON object has unexpected fields.
allowExtraFields :: JObjectDefinition e o a -> JObjectDefinition e o a
allowExtraFields (JObjectDefinition (Pair (Pair (Const objSchema) objValidate) objEncode)) =
  JObjectDefinition (Pair (Pair (Const objSchema') objValidate) objEncode)
  where
    objSchema' = objSchema { objSchemaAllowExtraFields = True }

-- | Create a 'JDefinition' for JSON objects:
--
-- @
-- data User =
--   MkUser { _userId :: UUID,
--            _userName :: Text,
--            _userAddr :: Text
--          }
--
-- 'Jijo.RecordField.TH.makeRecBuilder' \"_user\" ''User
--
-- jUser = 'defineJObject' $
--   pure recUser  -- recUser generated by 'Jijo.RecordField.TH.makeRecBuilder'
--     \<*\> 'jField' \@\"Id\"    \"id\"    jUUID
--     \<*\> 'jField' \@\"Email\" \"email\" jText
--     \<*\> 'jField' \@\"Name\"  \"name\"  jText
-- @
defineJObject :: JObjectDefinition e o o -> JDefinition e JSON.Value o
defineJObject objDefn = jObjectDefinition objDefn . jObject

-- | Create a 'JDefinition' for JSON objects, with an additional validation step.
defineJObjectEither :: JObjectDefinition e o (Either e o) -> JDefinition e JSON.Value o
defineJObjectEither objDefn = jObjectDefinitionEither objDefn . jObject

-- | Validate/encode a required object field.
inJField :: Text -> (o -> a) -> JDefinition e JSON.Value a -> JObjectDefinition e o a
inJField fieldName getField fieldDef = mkJObjectDefinition objSchema objValidate objEncode
  where
    objSchema = mempty { objSchemaFields = HashSet.singleton fieldName }
    objValidate = jValidateField fieldName (jValidate fieldDef)
    objEncode o = DList.singleton (fieldName, jEncode fieldDef (getField o))

-- | Validate/encode an optional object field.
inOptJField :: Text -> (o -> Maybe a) -> JDefinition e JSON.Value a -> JObjectDefinition e o (Maybe a)
inOptJField fieldName getField fieldDef = mkJObjectDefinition objSchema objValidate objEncode
  where
    objSchema = mempty { objSchemaFields = HashSet.singleton fieldName }
    objValidate = jValidateOptField fieldName (jValidate fieldDef)
    objEncode o = case getField o of
      Nothing -> DList.empty
      Just val -> DList.singleton (fieldName, jEncode fieldDef val)

-- | Validate/encode a required object field using the 'Field' machinery.
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

-- | Validate/encode an optional object field using the 'Field' machinery.
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
coerceJObjectDefinition (JObjectDefinition (Pair (Pair s p) q)) =
  JObjectDefinition (Pair (Pair (coerce s) (coerce p)) (coerce q))

----------------------------------------------------------------------------
-- Arrays
----------------------------------------------------------------------------

-- | Validate/encode a JSON array.
jArrayOf :: JDefinition e JSON.Value a -> JDefinition e JSON.Value (Vector a)
jArrayOf elementDefn = jDefinition validationArr encodingArr . jArray
  where
    validationArr = jValidateElements (jValidate elementDefn)
    encodingArr = Vector.map (jEncode elementDefn)

-- | Validate/encode a JSON array, via a list.
jListOf :: JDefinition e JSON.Value a -> JDefinition e JSON.Value [a]
jListOf elementDefn =
  jDefinition (pure . Vector.toList) Vector.fromList . jArrayOf elementDefn

----------------------------------------------------------------------------
-- Sums
----------------------------------------------------------------------------

-- | Validation/encoding of a sum constructor.
data JSumOption e a
  = JEnumOption a (a -> Bool)
  | forall b. JSumOption (b -> a) (a -> Maybe b) (JDefinition e JSON.Value b)

-- | An exception indicative of an invalid schema for a sum type. If you
-- encounter it, check that there are no duplicate labels in your inputs to
-- 'defineJSum'.
data JSumException
  = JSumNoEncoding
  | JSumAmbiguousEncoding
  deriving (Eq, Show)

instance Exception JSumException

-- | A constructor of a sum type with no attached data:
--
-- @
-- jSign = 'defineJSum' $
--   'jEnumOption' \"+\" _True <>
--   'jEnumOption' \"-\" _False
-- @
--
-- The above definition accepts JSON strings @\"+\"@ and @\"-\"@.
jEnumOption :: Text -> Prism' a () -> Map Text (JSumOption e a)
jEnumOption label p =
  Map.singleton label (JEnumOption (review p ()) (isJust . preview p))

-- | A constructor of a sum type with data attached:
--
-- @
-- jEither jLeft jRight = 'defineJSum' $
--   'jSumOption' \"left\"  _Left  jLeft  <>
--   'jSumOption' \"right\" _Right jRight
-- @
--
-- The above definition accepts JSON objects of the form @{ \"left\": ... }@ and @{ \"right\": ... }@.
jSumOption :: Text -> Prism' a b -> JDefinition e JSON.Value b -> Map Text (JSumOption e a)
jSumOption label p jDef =
  Map.singleton label (JSumOption (review p) (preview p) jDef)

-- | Create a 'JDefinition' for sum types:
--
-- @
--  jMaybe jJust = 'defineJSum' $
--    'jEnumOption' \"nothing\" _Nothing <>
--    'jSumOption' \"just\" _Just jJust
-- @
--
-- The above definition accepts JSON objects of the form @{ \"just\": ... }@
-- and the JSON string @\"nothing\"@.
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

-- | Validate/encode a JSON object.
jObject :: JDefinition e JSON.Value JSON.Object
jObject = jDefinition checkObject JSON.Object
  where
    checkObject = \case
      JSON.Object o -> pure o
      _ -> jValidationError (JTypeNotOneOf (Set.singleton JTyObject))

-- | Validate/encode a JSON array.
jArray :: JDefinition e JSON.Value JSON.Array
jArray = jDefinition checkArray JSON.Array
  where
    checkArray = \case
      JSON.Array a -> pure a
      _ -> jValidationError (JTypeNotOneOf (Set.singleton JTyArray))

-- | Validate/encode a JSON string.
jString :: JDefinition e JSON.Value Text
jString = jDefinition checkString JSON.String
  where
    checkString = \case
      JSON.String s -> pure s
      _ -> jValidationError (JTypeNotOneOf (Set.singleton JTyString))

-- | Validate/encode a JSON number.
jNumber :: JDefinition e JSON.Value Scientific
jNumber = jDefinition checkNumber JSON.Number
  where
    checkNumber = \case
      JSON.Number n -> pure n
      _ -> jValidationError (JTypeNotOneOf (Set.singleton JTyNumber))

-- | Validate/encode a JSON boolean.
jBool :: JDefinition e JSON.Value Bool
jBool = jDefinition checkBool JSON.Bool
  where
    checkBool = \case
      JSON.Bool b -> pure b
      _ -> jValidationError (JTypeNotOneOf (Set.singleton JTyBool))

-- | Validate/encode a nullable JSON value.
--
-- 'jNullable' assumses that the inner definition does not accept or produce
-- @null@. As a consequence, one should be careful not to nest this combinator:
--
-- @
-- jNullable (jNullable ...)  -- don't!
-- @
--
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
--
-- @
-- instance FromJSON Foo where
--   parseJSON = parseJSON_viaDefinition jFoo
-- @
--
parseJSON_viaDefinition ::
  JDefinition String JSON.Value a ->
  JSON.Value -> JSON.Parser a
parseJSON_viaDefinition d j =
  either (fail . renderJValidationReport) return $
  validateViaDefinition d j

-- | Default definition for 'JSON.ToJSON':
--
-- @
-- instance ToJSON Foo where
--   toJSON = toJSON_viaDefinition jFoo
-- @
--
toJSON_viaDefinition ::
  JDefinition e JSON.Value a ->
  a -> JSON.Value
toJSON_viaDefinition = jEncode

-- | A 'JDefinition' that arises from 'JSON.FromJSON' and 'JSON.ToJSON'.
--
-- prop> parseJSON_viaDefinition aesonJDefinition = parseJSON
-- prop> toJSON_viaDefinition aesonJDefinition = toJSON
--
aesonJDefinition ::
  (JSON.FromJSON a, JSON.ToJSON a) =>
  JDefinition String JSON.Value a
aesonJDefinition = jDefinition toB fromB
  where
    toB = eitherToJValidation . JSON.parseEither JSON.parseJSON
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
