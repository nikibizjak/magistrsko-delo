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
    (Reference owner ownee) : rest | owner == name -> freeVariable name rest
    head : rest -> head : freeVariable name rest

instance BorrowCheck Expression where
  borrowCheck sigma lifetimes ownership (Atom atom) =
    borrowCheck sigma lifetimes ownership atom
  
  borrowCheck sigma lifetimes ownership (FunctionApplication function _ arguments) =
    borrowCheckSequential sigma lifetimes ownership [function] [] arguments
  
  borrowCheck sigma lifetimes ownership (PrimitiveOperation _ arguments) =
    borrowCheckSequential sigma lifetimes ownership [] [] arguments

  borrowCheck sigma lifetimes ownership (LetIn name object body) = do

    -- The execution of a let expression is divided into 4 parts:
    --   1. Definition
    --   2. Object
    --   3. Body
    --   4. Epilogue

    -- DEFINITION: let name = object in ...
    -- Create a new lifetime. This lifetime is used as a lifetime for the
    -- defined variable (ie. in the expression `let name = object in body`, the
    -- variable name has a lifetime 'letLifetime).
    letLifetime <- freshLifetime
    let lifetimes' = (name, letLifetime) : lifetimes

    -- The only thing we know for now is that the variable `name` takes
    -- ownership of the heap object `object`.
    let ownership' = Owner name object : ownership

    -- OBJECT: let name = object in ...
    -- Analyze the ownership of the heap object `object`. 
    objectResult <- borrowCheck letLifetime lifetimes' ownership' object
    case objectResult of
      Left exception -> failure exception
      Right (objectLifetimes, objectOwnership, objectMoves, objectBorrows) -> do

        -- The variable `name` should take ownership of all the variables in
        -- `objectMoves`. It should also borrow all the variables in
        -- `objectBorrows`.
        let ownership'' = moveMany name objectMoves objectOwnership
        let ownership''' = borrowMany name objectBorrows ownership''

        -- BODY: let ... = ... in body
        -- Define a new unique variable `result`, which is the result of the let
        -- expression. Since it is the result, it should live in the parent
        -- lifetime `sigma`.
        resultVariable <- freshVariable "result"
        let lifetimes'' = (resultVariable, sigma) : objectLifetimes
        
        -- Analyze the ownership of the expression `body`.
        bodyResult <- borrowCheck letLifetime lifetimes'' ownership''' body
        case bodyResult of
          Left exception -> failure exception
          Right (lifetimesBody, ownershipBody, movesBody, borrowsBody) -> do

            -- ∀ variable ∈ movesBody: result -> variable
            let ownership' = moveMany resultVariable movesBody ownershipBody
            -- ∀ variable ∈ borrowsBody: result ~> variable
            let ownership'' = borrowMany resultVariable borrowsBody ownership'
            
            -- EPILOGUE: Executes right after the body of the let expression.
            -- Here, the memory for the heap objects is freed.

            -- Remove all variables that were created in this lifetime (that is
            -- all (name, lifetime) ∈ lifetimesBody).

            -- Find all variables that were defined in this lifetime.
            let lifetimeDefinitions = filter (\(_, lifetime) -> lifetime == letLifetime) lifetimesBody
            -- Get the names of the variables
            let variablesInLifetime = map fst lifetimeDefinitions

            -- Remove all the variables that were defined in this lifetime.
            let lifetimesFinal = filter (\(name, _) -> name `notElem` variablesInLifetime) lifetimesBody

            -- Free the variables that were defined in this lifetime.
            let ownershipFinal = foldr freeVariable ownership'' variablesInLifetime

            if isValid ownershipFinal lifetimesFinal
            then success (lifetimesFinal, ownershipFinal, [ resultVariable ], [ ])
            else throw "Dangling pointer"

  borrowCheck sigma lifetimes ownership (CaseOf scrutinee alternatives) = todo

instance BorrowCheck Object where
  borrowCheck sigma lifetimes ownership (Thunk expression) =
    borrowCheck sigma lifetimes ownership expression

instance BorrowCheck Binding where
  borrowCheck sigma lifetimes ownership (Binding name object) =
    borrowCheck sigma lifetimes ownership object

borrowCheckSequential sigma lifetimes ownership moves borrows items = do
  case items of
    [] -> success (lifetimes, ownership, moves, borrows)
    item : rest -> do
      result <- borrowCheck sigma lifetimes ownership item
      case result of
        Left exception -> return result
        Right (lifetimes', ownership', moves', borrows') ->
          borrowCheckSequential sigma lifetimes' ownership' moves' borrows' rest

borrowCheckProgram program =
  let
    -- The toplevel definitions all have 'static lifetime (which is internally
    -- represented as `Lifetime '0`).
    topLevelLifetimes = map (\(Binding name _) -> (name, staticLifetime)) program
    result = evalState (borrowCheckSequential staticLifetime topLevelLifetimes [] [] [] program) (1, 1)
  in
    case result of
      Right (lifetimes, ownership, _, _) -> Right (lifetimes, ownership)
      Left exception -> Left exception