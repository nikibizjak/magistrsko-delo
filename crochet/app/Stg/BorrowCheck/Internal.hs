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
  borrowCheck :: Lifetime -> [(Variable, Lifetime)] -> [Ownership] -> a -> State (Int, Int) (Either BorrowCheckException ([(Variable, Lifetime)], [Ownership], [Variable], [Variable]))

instance BorrowCheck Atom where
  borrowCheck _ lifetimes ownership (Literal _) =
    success (lifetimes, ownership, [], [])
  borrowCheck _ lifetimes ownership (Variable name) =
    success (lifetimes, ownership, [ name ], [])
  borrowCheck _ lifetimes ownership (Borrow name) =
    success (lifetimes, ownership, [], [ name ])

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

moveMany :: Foldable t => Variable -> t Variable -> [Ownership] -> [Ownership]
moveMany newOwner variables ownership =
  foldr (`takeOwnership` newOwner) ownership variables

borrowMany :: Variable -> [Variable] -> [Ownership] -> [Ownership]
borrowMany newOwner variables ownership =
  map (Reference newOwner) variables ++ ownership

isValid :: [Ownership] -> [(Variable, b)] -> Bool
isValid ownership lifetimes =
  case ownership of
    [] -> True
    Reference owner ownee : rest ->
      case lookup ownee lifetimes of
        Nothing -> False
        Just _ -> isValid rest lifetimes
    _ : rest ->
      isValid rest lifetimes

freeVariable name ownership =
  case ownership of
    [] -> []
    (Owner owner ownee) : rest | owner == name -> freeVariable name rest
    head : rest -> head : freeVariable name rest

instance BorrowCheck Expression where
  borrowCheck sigma lifetimes ownership (Atom atom) =
    borrowCheck sigma lifetimes ownership atom
  borrowCheck sigma lifetimes ownership (LetIn name object body) = do
    letLifetime <- freshLifetime

    let lifetimes' = (name, letLifetime) : lifetimes
    let ownership' = Owner name object : ownership

    -- Object
    definitionResult <- borrowCheck letLifetime lifetimes' ownership' object
    case definitionResult of
      Left exception -> failure exception
      Right (definitionLifetimes, definitionOwnership, definitionMoves, definitionBorrows) -> do

        -- name -> definitionMoves
        let ownership'' = moveMany name definitionMoves definitionOwnership
        -- name ~> definitionBorrows
        let ownership''' = borrowMany name definitionBorrows ownership''

        -- Body
        resultVariable <- freshVariable "result"
        let lifetimes'' = (resultVariable, sigma) : definitionLifetimes

        bodyResult <- borrowCheck letLifetime lifetimes'' ownership''' body
        case bodyResult of
          Left exception -> failure exception
          Right (lifetimesBody, ownershipBody, movesBody, borrowsBody) -> do

            -- result ~> borrowsBody
            let ownershipBody' = borrowMany resultVariable borrowsBody ownershipBody

            -- End
            --   1. Remove all variables that were created in this lifetime (that is
            --      all (name, lifetime) ∈ lifetimesBody).
            let lifetimeDefinitions = filter (\(_, lifetime) -> lifetime == letLifetime) lifetimesBody
            let variablesInLifetime = map fst lifetimeDefinitions

            let lifetimesFinal = filter (\(name, _) -> name `notElem` variablesInLifetime) lifetimesBody
            let ownershipFinal = takeOwnership name resultVariable ownershipBody'

            -- resultVariable -> movesBody
            let ownershipFinal' = foldr (`takeOwnership` resultVariable) ownershipFinal movesBody

            let ownershipFinal'' = foldr freeVariable ownershipFinal' variablesInLifetime

            if isValid ownershipFinal'' lifetimesFinal
              then success (lifetimesFinal, ownershipFinal'', [ resultVariable ], [ ])
              else throw "Dangling pointer"

  borrowCheck sigma lifetimes ownership (FunctionApplication function arity arguments) = todo
  borrowCheck sigma lifetimes ownership (PrimitiveOperation operation arguments) = todo
  borrowCheck sigma lifetimes ownership (CaseOf scrutinee alternatives) = todo

instance BorrowCheck Object where
  borrowCheck sigma lifetimes ownership (Thunk expression) =
    borrowCheck sigma lifetimes ownership expression

instance BorrowCheck Binding where
  borrowCheck sigma lifetimes ownership (Binding name object) =
    borrowCheck sigma lifetimes ownership object

borrowCheckSequential sigma lifetimes ownership items = do
  case items of
    [] -> success (lifetimes, ownership, [], [])
    item : rest -> do
      result <- borrowCheck sigma lifetimes ownership item
      case result of
        Left exception -> return result
        Right (lifetimes', ownership', _, _) ->
          borrowCheckSequential sigma lifetimes' ownership' rest

borrowCheckProgram program =
  let
    -- The toplevel definitions all have 'static lifetime (which is internally
    -- represented as `Lifetime '0`).
    topLevelLifetimes = map (\(Binding name _) -> (name, staticLifetime)) program
    result = evalState (borrowCheckSequential staticLifetime topLevelLifetimes [] program) (1, 1)
  in
    case result of
      Right (lifetimes, ownership, _, _) -> Right (lifetimes, ownership)
      Left exception -> Left exception