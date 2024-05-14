module Stg.BorrowCheck.Lifetime where

import Control.Monad.State

-- Lifetimes
newtype Lifetime
  = Lifetime Int
  deriving (Eq, Ord)

staticLifetime = Lifetime 0

instance Show Lifetime where
  show (Lifetime 0) = "'static"
  show (Lifetime n) = '\'' : show n

-- Generate a new unique lifetime that hasn't been used yet.
fresh :: State Int Lifetime
fresh = do
  n <- get
  put (n + 1)
  return (Lifetime n)