module Jijo.PathSpec (spec) where

import Control.Lens hiding ((.=))

import Test.Hspec

import Jijo.Path

spec :: Spec
spec = do
  test_building
  test_rendering

test_building :: Spec
test_building = describe "Building" $ do
  it "emptyJPathBuilder == []" $ do
    buildJPath emptyJPathBuilder `shouldBe` JPath []
  it "addJPathSegment works" $ do
    buildJPath (emptyJPathBuilder
                  & addJPathSegment (JPSField "foo")
                  & addJPathSegment (JPSField "bar"))
      `shouldBe` JPath [JPSField "foo", JPSField "bar"]

test_rendering :: Spec
test_rendering = describe "Rendering" $ do
  it "empty" $ do
    renderJPath (JPath []) `shouldBe` "$"
  it "index" $ do
    renderJPath (JPath [JPSIndex 0]) `shouldBe` "$[0]"
  it "simple key" $ do
    renderJPath (JPath [JPSField "foo"]) `shouldBe` "$.foo"
  it "weird key" $ do
    renderJPath (JPath [JPSField "'"]) `shouldBe` "$['\\'']"
  it "empty key" $ do
    renderJPath (JPath [JPSField ""]) `shouldBe` "$['']"
  it "numeric key" $ do
    renderJPath (JPath [JPSField "10"]) `shouldBe` "$['10']"
  it "key + key" $ do
    renderJPath (JPath [JPSField "foo", JPSField "bar"]) `shouldBe` "$.foo.bar"
  it "key + index" $ do
    renderJPath (JPath [JPSField "foo", JPSIndex 10]) `shouldBe` "$.foo[10]"
  it "index + index" $ do
    renderJPath (JPath [JPSIndex 5, JPSIndex 10]) `shouldBe` "$[5][10]"
