{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.ValidationSpec (spec) where

import Common.Validation
import Control.Monad.Writer (runWriter)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

spec :: Spec
spec = do
  describe ".$" $ do
    it "maps over a Just without recording any error" $
      runWriter (Just (3 :: Int) .$ (+ 1)) `shouldBe` (Just 4, [])
    it "passes Nothing through untouched" $
      runWriter ((Nothing :: Maybe Int) .$ (+ 1)) `shouldBe` (Nothing, [])

  describe "isThere" $ do
    it "passes a present value through with no error" $
      runWriter (isThere "missing" (Just (5 :: Int))) `shouldBe` (Just 5, [])
    it "records the given error and returns Nothing for a missing value" $
      runWriter (isThere "missing" (Nothing :: Maybe Int)) `shouldBe` (Nothing, ["missing"])
    prop "never errors on a Just, for any value" $ \(x :: Int) ->
      runWriter (isThere "missing" (Just x)) === (Just x, [])

  describe "isNotEmpty" $ do
    it "passes a non-empty value through with no error" $
      runWriter (isNotEmpty "empty" (Just ("hi" :: String))) `shouldBe` (Just "hi", [])
    it "still returns the value but records an error when it's empty" $
      runWriter (isNotEmpty "empty" (Just ("" :: String))) `shouldBe` (Just "", ["empty"])
    it "passes Nothing through with NO error -- it only checks emptiness, not presence" $
      runWriter (isNotEmpty "empty" (Nothing :: Maybe String)) `shouldBe` (Nothing, [])

  describe "valRead" $ do
    it "parses a valid value with no error" $
      (runWriter (valRead "bad int" (Just "42")) :: (Maybe Int, [ValidationErr]))
        `shouldBe` (Just 42, [])
    it "records an error and returns Nothing for an unparseable value" $
      (runWriter (valRead "bad int" (Just "not-a-number")) :: (Maybe Int, [ValidationErr]))
        `shouldBe` (Nothing, ["bad int"])
    it "passes Nothing through with NO error" $
      (runWriter (valRead "bad int" Nothing) :: (Maybe Int, [ValidationErr]))
        `shouldBe` (Nothing, [])

  describe "isBetween" $ do
    it "passes an in-range value through with no error" $
      runWriter (isBetween 1 10 "out of range" (Just (5 :: Int))) `shouldBe` (Just 5, [])
    it "still returns the value but records an error when out of range" $
      runWriter (isBetween 1 10 "out of range" (Just (99 :: Int)))
        `shouldBe` (Just 99, ["out of range"])
    it "passes Nothing through with NO error" $
      runWriter (isBetween 1 10 "out of range" (Nothing :: Maybe Int)) `shouldBe` (Nothing, [])

  describe "runValidation" $ do
    it "succeeds when the value is present and no errors were recorded" $
      runValidation id (isThere "missing" (Just (1 :: Int)))
        `shouldBe` (Right 1 :: Either [ValidationErr] Int)

    it "fails with the recorded errors even when the value is still Just -- this is what \
       \makes isBetween/isNotEmpty/isEq's \"pass the value through but still record an \
       \error\" behavior actually reject the input overall" $
      runValidation id (isBetween 1 10 "out of range" (Just (99 :: Int)))
        `shouldBe` (Left ["out of range"] :: Either [ValidationErr] Int)

    it "falls back to a generic \"Unknown error\" for a (Nothing, []) result -- reachable \
       \when a Nothing-passthrough check (isNotEmpty/isBetween/isEq/valRead) runs on a \
       \Nothing that was never actually flagged by isThere first" $
      runValidation id (isNotEmpty "empty" (Nothing :: Maybe String))
        `shouldBe` (Left ["Unknown error in validation"] :: Either [ValidationErr] String)

  describe "errcat" $
    it "concatenates a String key with a Text message" $
      errcat "WEB_PORT" " is missing" `shouldBe` "WEB_PORT is missing"
