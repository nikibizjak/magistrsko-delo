module Stg.BorrowCheck.Exception where

import Control.Monad.State

-- Define exception for borrow checking
newtype BorrowCheckException
  = BorrowCheckException String
  deriving (Show)

throw :: Monad m => String -> m (Either BorrowCheckException b)
throw text = return $ Left $ BorrowCheckException text

todo :: State Int (Either BorrowCheckException b)
todo = throw "Not implemented yet"