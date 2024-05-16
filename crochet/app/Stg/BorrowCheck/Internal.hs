module Stg.BorrowCheck.Internal where

import Stg.BorrowCheck.Lifetime
import Stg.BorrowCheck.Exception
import Stg.BorrowCheck.Moves
import Stg.BorrowCheck.Borrows
import Stg.Stg
import Stg.Pretty
import Control.Monad.State (evalState, State)
import Debug.Trace
import Data.Foldable (toList)

data Ownership
  = Owner Variable Object
  | Reference Variable Variable

instance Show Ownership where
  show (Owner owner ownee) = owner ++ " -> " ++ pretty ownee
  show (Reference owner ownee) = owner ++ " ~> " ++ ownee

class BorrowCheck a where
  borrowCheck :: Lifetime -> [(Variable, Lifetime)] -> [Ownership] -> a -> State (Int, Int) (Either BorrowCheckException ([(Variable, Lifetime)], [Ownership]))

instance BorrowCheck Atom where
  borrowCheck _ lifetimes ownership _ = success (lifetimes, ownership)

owner ownership =
  case ownership of
    Owner owner _ -> owner
    Reference owner _ -> owner

takeOwnership previousOwner newOwner ownership =
  case ownership of
    [] -> []
    (Reference owner ownee) : rest | owner == previousOwner ->
      Reference newOwner ownee : takeOwnership previousOwner newOwner rest
    (Owner owner object) : rest | owner == previousOwner ->
      Owner newOwner object : takeOwnership previousOwner newOwner rest
    head : rest ->
      head : takeOwnership previousOwner newOwner rest

isValid ownership lifetimes =
  case ownership of
    [] -> True
    Reference owner ownee : rest ->
      case lookup ownee lifetimes of
        Nothing -> False
        Just _ -> isValid rest lifetimes
    _ : rest ->
      isValid rest lifetimes

instance BorrowCheck Expression where
  borrowCheck sigma lifetimes ownership (Atom atom) = todo
  borrowCheck sigma lifetimes ownership (LetIn name object body) = todo
  borrowCheck sigma lifetimes ownership (FunctionApplication function arity arguments) = todo
  borrowCheck sigma lifetimes ownership (PrimitiveOperation operation arguments) = todo
  borrowCheck sigma lifetimes ownership (CaseOf scrutinee alternatives) = todo

borrowCheckExpression sigma lifetimes ownership expression = do
  case expression of
    Atom atom -> borrowCheck sigma lifetimes ownership atom
    LetIn name object body -> do
      lifetime <- freshLifetime
      let lifetimes' = (name, lifetime) : lifetimes
      let ownership' = traceShow body traceShow (borrows body) Owner name object : ownership

      let definitionBorrows = map (Reference name) (toList $ borrows object)

      bodyResult <- borrowCheckExpression lifetime lifetimes' (definitionBorrows ++ ownership') body
      case bodyResult of
        Right (lifetimes'', ownership'') -> do
          resultVariable <- freshVariable "result"
          let
            resultBorrows = map (Reference resultVariable) (toList $ borrows body)
            ownership''' = takeOwnership name resultVariable ownership''
            lifetimes''' = filter (\(variable, _) -> variable /= name) lifetimes''

            finalLifetimes = (resultVariable, sigma) : lifetimes'''
            finalOwnership = resultBorrows ++ ownership'''
            in

              -- Check if there is any (Reference ... name) where name is not
              -- defined in the finalLifetimes.
              if isValid finalOwnership finalLifetimes
                then success (finalLifetimes, finalOwnership)
                else throw "Dangling pointer"
        Left exception -> return $ Left exception

instance BorrowCheck Object where
  borrowCheck sigma lifetimes ownership (Thunk expression) =
    borrowCheckExpression sigma lifetimes ownership expression

instance BorrowCheck Binding where
  borrowCheck sigma lifetimes ownership (Binding name object) =
    borrowCheck sigma lifetimes ownership object

borrowCheckSequential sigma lifetimes ownership items = do
  case items of
    [] -> success (lifetimes, ownership)
    item : rest -> do
      result <- borrowCheck sigma lifetimes ownership item
      case result of
        Left exception -> return result
        Right (lifetimes', ownership') ->
          borrowCheckSequential sigma lifetimes' ownership' rest

borrowCheckProgram program =
  let
    -- The toplevel definitions all have 'static lifetime (which is internally
    -- represented as `Lifetime '0`).
    topLevelLifetimes = map (\(Binding name _) -> (name, staticLifetime)) program
    result = evalState (borrowCheckSequential staticLifetime topLevelLifetimes [] program) (1, 1)
  in
    case result of
      Right (lifetimes, ownership) -> Right (lifetimes, ownership)
      Left exception -> Left exception