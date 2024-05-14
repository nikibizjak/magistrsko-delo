module Stg.BorrowCheck.Internal where

import Stg.BorrowCheck.Lifetime
import Stg.BorrowCheck.Exception
import Stg.Stg

-- A borrow check context is a mapping from a name (variable) to the lifetime in
-- which the name was introduced.
type Context = [ (String, Lifetime) ]

class BorrowCheck a where
  borrowCheck :: Context -> Lifetime -> a -> Lifetime

borrowCheckProgram :: Program -> Either a ((), ())
borrowCheckProgram program =
  -- The toplevel definitions all have 'static lifetime (which is internally
  -- represented as `Lifetime '0`).
  let
    topLevelContext = map (\(Binding name _) -> (name, staticLifetime)) program
  in
    Right ((), ())