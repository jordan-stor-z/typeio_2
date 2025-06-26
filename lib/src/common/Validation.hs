{-# LANGUAGE OverloadedStrings #-}

module Common.Validation where

import Control.Monad             (when)
import Control.Monad.Writer      (runWriter, tell, Writer)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT, hoistMaybe)
import Data.Text                 (Text)
import Text.Read                 (readMaybe)

type ErrMsg = Text
type ValidationErr = Text

validate :: Maybe a -> Writer [ValidationErr] (Maybe a)
validate = pure 

(.$) :: Maybe a -> (a -> b) -> Writer [ValidationErr] (Maybe b)
m .$ f = pure $ fmap f m

isThere :: ErrMsg -> Maybe a -> Writer [ValidationErr] (Maybe a)
isThere e m = 
  case m of
    Nothing -> tell [e] >> return Nothing
    Just x  -> return $ Just x

isNotEmpty :: (Eq a, Monoid a) => ErrMsg -> Maybe a -> Writer [ValidationErr] (Maybe a)
isNotEmpty e m = runMaybeT $ do
  v <- hoistMaybe m
  when (v == mempty) $ do
    lift $ tell [e]
  return v

valRead :: (Read b, Show a) 
  => ErrMsg
  -> Maybe a
  -> Writer [ValidationErr] (Maybe b)
valRead e m = do
  let r = m >>= readMaybe . show
  case r of
    Nothing -> tell [e] >> return Nothing
    Just x  -> return $ Just x

isBetween :: (Ord a) => a -> a -> Text -> Maybe a -> Writer [ValidationErr] (Maybe a)
isBetween minV maxV e m = runMaybeT $ do
  v <- hoistMaybe m
  when (v < minV || v > maxV) $ do
    lift $ tell [e]
  return v

runValidation :: ([ValidationErr] -> b)
  -> Writer [ValidationErr] (Maybe a) 
  -> Either b a 
runValidation f w = 
  case res of
    (Just x, []) -> Right x
    (Nothing, []) -> Left $ f ["Unknown error in validation"]
    (_, es)       -> Left $ f es
  where res = runWriter w

