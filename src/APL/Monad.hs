module APL.Monad
  ( failure,
    FinchID,
    meet,
    groom,
    ignore,
    EvalM,
    EvalOp (..),
    Free (..),
    Error,
  )
where

import Control.Monad (ap)

type Error = String

data Free e a
  = Pure a
  | Free (e (Free e a))

instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free g) = Free $ fmap (fmap f) g

instance (Functor e) => Applicative (Free e) where
  pure = Pure
  (<*>) = ap

instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  Free g >>= f = Free $ h <$> g
    where
      h x = x >>= f

type FinchID = Int

data EvalOp a
  = ErrorOp Error
  | MeetOp (FinchID -> a)
  | GroomOp (Bool -> a)
  | IgnoreOp (Bool -> a)

instance Functor EvalOp where
  fmap _ (ErrorOp e) = ErrorOp e
  fmap f (GroomOp k) = GroomOp $ f . k
  fmap f (IgnoreOp k) = IgnoreOp $ f . k
  fmap f (MeetOp k) = MeetOp $ f . k

type EvalM a = Free EvalOp a

failure :: String -> EvalM a
failure = Free . ErrorOp

meet :: EvalM Int
meet = Free $ MeetOp pure

groom :: EvalM Bool
groom = Free $ GroomOp pure

ignore :: EvalM Bool
ignore = Free $ IgnoreOp pure
