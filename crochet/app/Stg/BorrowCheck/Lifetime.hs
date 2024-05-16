module Stg.BorrowCheck.Lifetime where

import Control.Monad.State
import Stg.Stg ( Variable )

-- Lifetimes
newtype Lifetime
  = Lifetime Int
  deriving (Eq, Ord)

staticLifetime = Lifetime 0

instance Show Lifetime where
  show (Lifetime 0) = "'static"
  show (Lifetime n) = '\'' : show n

-- Generate a new unique lifetime that hasn't been used yet.
freshVariable :: String -> State (Int, Int) Variable
freshVariable prefix = do
  (lifetime, variable) <- get
  put (lifetime, variable + 1)
  return $ prefix ++ show variable

freshLifetime :: State (Int, Int) Lifetime
freshLifetime = do
  (lifetime, variable) <- get
  put (lifetime + 1, variable)
  return (Lifetime lifetime)