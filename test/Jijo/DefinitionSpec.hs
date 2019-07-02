{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Jijo.DefinitionSpec (spec) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Lens hiding ((.=))
import Data.Void
import Data.Aeson as JSON hiding (encode)
import Data.Aeson.Lens as JSON
import Data.Set as Set
import Data.Scientific as Scientific
import Data.Vector as Vector
import Data.Text (Text, toLower)

import Test.Hspec
import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import HaskellWorks.Hspec.Hedgehog

import Jijo.RecordField.TH
import Jijo.Path
import Jijo.Definition

data Pair a b = Pair { _pairFst :: a, _pairSnd :: b }
  deriving (Eq, Show)

makeRecBuilder "_pair" ''Pair

spec :: Spec
spec = do
  test_stock
  test_objects
  test_composition
  test_errors
  test_sums
  test_nullable

test_stock :: Spec
test_stock = describe "Stock definitions" $ do
  describe "jObject" $ do
    it "accepts only objects" $ require $ prop_accepts JTyObject _Object jObject
    it "encodes into an object" $ require $ prop_encodes genJObject _Object jObject
  describe "jArray" $ do
    it "accepts only arrays" $ require $ prop_accepts JTyArray _Array jArray
    it "encodes into an array" $ require $ prop_encodes genJArray _Array jArray
  describe "jString" $ do
    it "accepts only strings" $ require $ prop_accepts JTyString _String jString
    it "encodes into a string" $ require $ prop_encodes genJString _String jString
  describe "jNumber" $ do
    it "accepts only numbers" $ require $ prop_accepts JTyNumber _Number jNumber
    it "encodes into a number" $ require $ prop_encodes genJNumber _Number jNumber
  describe "jBool" $ do
    it "accepts only bools" $ require $ prop_accepts JTyBool _Bool jBool
    it "encodes into a bool" $ require $ prop_encodes genJBool _Bool jBool
  where
    -- Check that the definition only accepts JSON values of a certain type
    prop_accepts ::
      (Eq a, Show a) =>
      JTy ->
      APrism' JSON.Value a ->
      JDefinition Void JSON.Value a ->
      Property
    prop_accepts ty ctr d = property $ do
      j <- forAll genJValue
      let val = validateViaDefinition d j
      case j ^? clonePrism ctr of
        Just x -> val === Right x
        _ -> val === Left (JValidationReport [JTypeNotOneOf (Set.singleton ty)] mempty)

    -- Check that the definition only encodes into JSON values of a certain type
    prop_encodes ::
      (Eq a, Show a) =>
      Gen JSON.Value ->
      APrism' JSON.Value a ->
      JDefinition Void JSON.Value a ->
      Property
    prop_encodes gen ctr d = property $ do
      j <- forAll gen
      Just j === fmap (encodeViaDefinition d) (j ^? clonePrism ctr)

test_objects :: Spec
test_objects = describe "Object definition machinery" $ do
  it "encodes" $
    encode (Pair True (Just False))
      `shouldBe` object ["foo" .= True, "bar" .= False]
  it "decodes" $
    validate (object ["foo" .= True, "bar" .= False])
      `shouldBe` Right (Pair True (Just False))
  it "fails when the value is not an object" $ do
    validate (JSON.Bool True)
      `shouldBe` Left [(JPath [], JTypeNotOneOf (Set.fromList [JTyObject]))]
  describe "inJField" $ do
    it "reports an error when the field is not found" $
      validate (object ["bar" .= True])
        `shouldBe` Left [(JPath [], JMissingField "foo")]
    it "reports an error when the field is unparseable" $
      validate (object ["foo" .= (), "bar" .= True])
        `shouldBe` Left [(JPath [JPSField "foo"], JTypeNotOneOf (Set.fromList [JTyBool]))]
  describe "inOptJField" $ do
    it "succeeds when the field is not found" $
      validate (object ["foo" .= True, "bar" .= False])
        `shouldBe` Right (Pair True (Just False))
    it "reports an error when the field is unparseable" $
      validate (object ["foo" .= True, "bar" .= ()])
        `shouldBe` Left [(JPath [JPSField "bar"], JTypeNotOneOf (Set.fromList [JTyBool]))]
    -- TODO: do we want to change this?
    it "doesn't allow null in place of a missing field" $
      validate (object ["foo" .= True, "bar" .= JSON.Null])
        `shouldBe` Left [(JPath [JPSField "bar"], JTypeNotOneOf (Set.fromList [JTyBool]))]
    it "doesn't encode anything when the field is missing" $
      encode (Pair True Nothing)
        `shouldBe` object ["foo" .= True]
  where
    sampleDefn :: JDefinition Void Value (Pair Bool (Maybe Bool))
    sampleDefn = defineJObject $
      recPair <$> jField "foo" jBool <*> jFieldOpt "bar" jBool
    validate =
      over _Left flattenJValidationReport .
      validateViaDefinition sampleDefn
    encode = encodeViaDefinition sampleDefn

test_composition :: Spec
test_composition = describe "Composition of definitions" $ do
  it "encoding works" $
    encode True `shouldBe` JSON.Bool True
  it "encoding works even when the condition is violated" $
    encode False `shouldBe` JSON.Bool False
  it "decoding works" $
    validate (JSON.Bool True) `shouldBe` Right True
  it "errors from the inner validator are present" $
    validate (JSON.String "blah")
      `shouldBe` Left [(JPath [], JTypeNotOneOf (Set.fromList [JTyBool]))]
  it "errors from the outer validator are present" $
    validate (JSON.Bool False)
      `shouldBe` Left [(JPath [], JValidationFail "bad")]
  where
    sampleDefn :: JDefinition Text Value Bool
    sampleDefn =
      jDefinition (\x -> if x then pure x else jValidationFail "bad") id .
      jBool
    validate =
      over _Left flattenJValidationReport .
      validateViaDefinition sampleDefn
    encode = encodeViaDefinition sampleDefn

test_errors :: Spec
test_errors = describe "Validation errors" $ do
  it "multiple errors are reported" $
    validate (object [])
      `shouldBe` Left [(JPath [], JMissingField "eq"),
                       (JPath [], JMissingField "neq")]
  it "nested errors are reported correctly" $ do
    validate (object ["eq" .= object ["a" .= True, "b" .= False],
                      "neq" .= object ["b" .= ()]])
      `shouldBe` Left [(JPath [JPSField "eq"], JValidationFail "!="),
                       (JPath [JPSField "neq"], JMissingField "a"),
                       (JPath [JPSField "neq", JPSField "b"], JTypeNotOneOf (Set.fromList [JTyBool]))]
  where
    sampleDefn :: JDefinition Text Value (Pair (Pair Bool Bool) (Pair Bool Bool))
    sampleDefn = defineJObject $ do
      eq <- jField "eq" $
        jDefinition (\p@(Pair a b) -> if a == b then pure p else jValidationFail "!=") id .
        defineJObject (recPair <$> jField "a" jBool <*> jField "b" jBool)
      neq <- jField "neq" $
        jDefinition (\p@(Pair a b) -> if a /= b then pure p else jValidationFail "==") id .
        defineJObject (recPair <$> jField "a" jBool <*> jField "b" jBool)
      pure (recPair eq neq)
    validate =
      over _Left flattenJValidationReport .
      validateViaDefinition sampleDefn

test_sums :: Spec
test_sums = describe "Sum definition machinery" $ do
  it "encodes enum options" $
    encode Nothing `shouldBe` JSON.String "nothing"
  it "encodes sum options" $
    encode (Just True) `shouldBe` object ["just" .= JSON.Bool True]
  it "validates enum options" $
    validate (JSON.String "nothing") `shouldBe` Right Nothing
  it "validates sum options" $
    validate (object ["just" .= JSON.Bool False]) `shouldBe` Right (Just False)
  it "reports non-label types" $
    validate JSON.Null `shouldBe`
      Left [(JPath [], JTypeNotOneOf (Set.fromList [JTyString, JTyObject]))]
  it "reports bad labels" $
    validate (JSON.String "none") `shouldBe`
      Left [(JPath [], JLabelNotOneOf (Set.fromList ["nothing", "just"]))]
  it "reports malformed sums" $
    validate (object []) `shouldBe`
      Left [(JPath [], JMalformedSum)]
  where
    sampleDefn :: JDefinition Text JSON.Value (Maybe Bool)
    sampleDefn = defineJSum $
      jEnumOption "nothing" _Nothing <>
      jSumOption "just" _Just jBool
    validate =
      over _Left flattenJValidationReport .
      validateViaDefinition sampleDefn
    encode = encodeViaDefinition sampleDefn

data Greeting = Hello | Hi
  deriving (Eq, Show)

jGreeting :: JDefinition () JSON.Value Greeting
jGreeting =
    jDefinition (validateGreeting . toLower) encodeGreeting . jString
  where
    encodeGreeting Hello = "hello"
    encodeGreeting Hi = "hi"

    validateGreeting "hello" = pure Hello
    validateGreeting "hi" = pure Hi
    validateGreeting _ = jValidationFail ()

test_nullable :: Spec
test_nullable = describe "Nullable fields" $ do
  it "encodes null" $
    encode Nothing `shouldBe` JSON.Null
  it "encodes non-null" $
    encode (Just Hi) `shouldBe` JSON.String "hi"
  it "validates null" $
    validate JSON.Null `shouldBe` Right Nothing
  it "validates non-null" $
    validate (JSON.String "HeLLo") `shouldBe` Right (Just Hello)
  it "keeps original error" $
    validate (JSON.String "nope") `shouldBe`
      Left [(JPath [], JValidationFail ())]
  it "reports that null was expected" $
    validate (JSON.Bool True) `shouldBe`
      Left [(JPath [], JTypeNotOneOf (Set.fromList [JTyString, JTyNull]))]
  where
    sampleDefn :: JDefinition () JSON.Value (Maybe Greeting)
    sampleDefn = jNullable jGreeting
    validate =
      over _Left flattenJValidationReport .
      validateViaDefinition sampleDefn
    encode = encodeViaDefinition sampleDefn


----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

genJNull :: Gen JSON.Value
genJNull = pure JSON.Null

genJString :: Gen JSON.Value
genJString = JSON.String <$> Gen.text (Range.constant 0 5) Gen.unicode

genJBool :: Gen JSON.Value
genJBool = JSON.Bool <$> Gen.bool

genJNumber :: Gen JSON.Value
genJNumber = JSON.Number . Scientific.fromFloatDigits <$>
  Gen.double (Range.constant (-100) 100)

genJArray :: Gen JSON.Value
genJArray = do
  let gen = Gen.recursive Gen.choice
              [genJBool, genJNumber, genJString]
              [genJArray, genJObject]
  (JSON.Array . Vector.fromList) <$> Gen.list (Range.constant 0 5) gen

genJObject :: Gen JSON.Value
genJObject = JSON.object <$>
  Gen.list (Range.constant 0 5)
           ((,) <$> Gen.text (Range.constant 0 5) Gen.unicode <*> genJValue)

genJValue :: Gen JSON.Value
genJValue =
  Gen.choice [genJNull, genJString, genJBool, genJNumber, genJArray, genJObject]
