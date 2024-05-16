module Stg.BorrowCheck.Exception where

import Control.Monad.State

-- Define exception for borrow checking
newtype BorrowCheckException
  = BorrowCheckException String
  deriving (Show)

success :: Monad m => b -> m (Either a b)
success value = do
  return $ Right value

throw :: Monad m => String -> m (Either BorrowCheckException b)
throw text = return $ Left $ BorrowCheckException text

todo :: State a (Either BorrowCheckException b)
todo = throw "Not implemented yet"