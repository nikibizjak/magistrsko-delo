module Stg.BorrowCheck (borrowCheckProgram, BorrowCheckException(..)) where

import Stg.BorrowCheck.Internal hiding (borrowCheckProgram)
import Stg.BorrowCheck.Exception

import Stg.Stg
import Stg.BorrowCheck.Lifetime

borrowCheckProgram :: Program -> Either BorrowCheckException ([(Variable, Lifetime)], [Ownership])
borrowCheckProgram program =
    Right ([], [])