{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Text.UtilSpec (spec) where

import Data.Text (pack)
import Data.Text.Util (intToText)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

spec :: Spec
spec = describe "intToText" $ do
  it "renders a positive Int the same as show" $
    intToText (42 :: Int) `shouldBe` "42"
  it "renders zero" $
    intToText (0 :: Int) `shouldBe` "0"
  it "works for any Integral, not just Int (e.g. Integer)" $
    intToText (123456789012345 :: Integer) `shouldBe` "123456789012345"
  prop "always matches Data.Text.pack . show, for any Int" $
    \(n :: Int) -> intToText n === pack (show n)
