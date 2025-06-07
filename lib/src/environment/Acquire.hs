module Environment.Acquire where

import Control.Exception (SomeException)

newtype Acquire a = Acquire 
  { runAcquire :: [IO ()] -> IO (Either SomeException (a, [IO ()]))
  }

data Acquire2 a = Success a [IO ()] | Failure [SomeException] [IO ()]

instance Functor Acquire2 where
  fmap f (Success a cl)  = Success (f a) cl
  fmap _ (Failure es cl) = Failure es cl

instance Applicative Acquire2 where
  pure a = Success a []
  Success f cl <*> Success a cl'    = Success (f a) (cl ++ cl')
  Success _ cl <*> Failure es cl'   = Failure es (cl ++ cl')
  Failure es cl <*> Success _ cl'   = Failure es (cl ++ cl')
  Failure es cl <*> Failure es' cl' = Failure (es ++ es') (cl ++ cl')

instance Monad Acquire2 where
  return = pure
  Success a cl >>= f = 
    let m = f a 
    in case m of
      Success b cl' -> Success b (cl ++ cl')
      Failure es cl' -> Failure es (cl ++ cl')
  Failure es cl >>= _ = Failure es cl

fromEither :: Either SomeException (a, IO ()) -> Acquire2 a
fromEither (Left e) = Failure [e] []
fromEither (Right (a, cl)) = Success a [cl]

