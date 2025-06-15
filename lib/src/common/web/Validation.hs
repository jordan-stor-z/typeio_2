{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common.Web.Validation where

import Control.Monad.Trans.Writer (WriterT)

newtype ValidationT m a = ValidationT 
  { runValidationT :: WriterT [String] m a
  } deriving (Functor, Applicative, Monad)

