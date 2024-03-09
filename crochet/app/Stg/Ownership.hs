module Stg.Ownership where
import Stg.Stg
import Stg.Pretty
import Stg.NameResolution (name)
import Data.List (find)

type Constraint = (String, String)
type Constraints = [Constraint]

ownership :: [Binding] -> Constraints
ownership program =
    let
        initialConstraints = map (\(Binding name _) -> (name, name)) program
    in
        foldr ownershipBinding initialConstraints program

ownershipBinding :: Binding -> Constraints -> Constraints
ownershipBinding (Binding name object) = ownershipObject name object

ownershipObject :: String -> Object -> Constraints -> Constraints
ownershipObject owner object constraints =
    case object of
        Thunk expression -> ownershipExpression owner expression constraints
        Constructor _ _ -> constraints -- TODO
        Function _ _ -> constraints -- TODO
        PartialApplication _ _ -> constraints -- TODO
        BlackHole -> constraints

ownershipExpression :: String -> Expression -> Constraints -> Constraints
ownershipExpression owner expression constraints =
    case expression of
        Atom atom -> ownershipAtom owner atom constraints
        LetIn name object body ->
            let constraints' = ownershipObject name object constraints in
                ownershipExpression owner body constraints'
        FunctionApplication function arity arguments ->
            let
                constraints' = ownershipExpression owner (Atom $ Variable function) constraints
            in
                foldr (ownershipAtom owner) constraints' arguments
        PrimitiveOperation _ arguments ->
            foldr (ownershipAtom owner) constraints arguments
        CaseOf scrutinee alternatives ->
            -- TODO: Implement this
            -- 
            -- Since only one of the alternatives will ever be executed, the
            -- same variable is allowed to be moved into multiple branches at
            -- the same time. But we **must** make sure that variable is moved
            -- into scrutinee and branches at the same time!
            ownershipExpression owner scrutinee constraints

ownershipAlternative :: String -> Alternative -> Constraints -> Constraints
ownershipAlternative owner alternative constraints =
    case alternative of
        AlgebraicAlternative _ variables body -> constraints
        DefaultAlternative variable body -> constraints

ownershipAtom :: String -> Atom -> Constraints -> Constraints
ownershipAtom owner atom constraints =
    case atom of
        Literal _ -> constraints
        Variable (ReferencedVariable _) -> constraints
        Variable (BorrowedVariable name) ->
            case findOwner constraints name of
                -- Everything is fine, the variable doesn't have an owner. We
                -- can proceed.
                Nothing -> (owner, name) : constraints
                -- The variable has already been moved. It can't be moved again!
                Just (foundOwner, _) ->
                    error ("Can not move variable '" ++ name ++ "' to variable '" ++ owner ++ "' because it has already been moved to variable '" ++ foundOwner ++ "'.")

findOwner constraints name =
    find (\(owner, ownee) -> name == ownee) constraints