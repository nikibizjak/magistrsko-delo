module Stg.MoveCheck where

import Stg.Stg
import qualified Data.Set as Set
import Stg.Pretty

newtype MoveCheckException = MoveCheckException String

-- Move check context contains mappings (source -> destination), where mapping
-- (source, destination) means that the variable source was moved to variable
-- destination.
-- A move of variable x is valid only if x has not been moved yet. That is
-- exactly when (x, _) is not an element of the move context.
type MoveCheckContext = [ (String, Expression) ]

class MoveCheck a where
  moveCheck :: MoveCheckContext -> Maybe Expression -> a -> Either MoveCheckException MoveCheckContext

todo = Left (MoveCheckException "TODO")

instance MoveCheck Object where
    moveCheck context owner (Function parameters body) =
        -- TODO: Only parameters which have been moved can be moved again?
        -- moveCheck context owner body
        todo
    moveCheck context owner (PartialApplication function arguments) =
        -- TODO: 
        todo
    moveCheck context owner (Constructor name arguments) =
        moveCheckSequential context owner arguments
    moveCheck context owner (Thunk expression) = moveCheck context owner expression
    moveCheck context owner BlackHole = Right context

instance MoveCheck Expression where
    moveCheck context owner expression =
        case expression of
            Atom atom -> moveCheck context (Just expression) atom
            FunctionApplication function arity arguments ->
                -- TODO: Fix this
                -- case moveCheck context (Just expression) function of
                --     Left exception -> Left exception
                --     Right context' -> moveCheckSequential context (Just expression) arguments
                moveCheckSequential context (Just expression) arguments
            PrimitiveOperation operation arguments ->
                moveCheckSequential context (Just expression) arguments
            LetIn name value body ->
                case moveCheck context (Just expression) value of
                    Left exception -> Left exception
                    Right context' ->
                        moveCheck context' (Just expression) body
            CaseOf scrutinee alternatives ->
                case moveCheck context (Just expression) scrutinee of
                    Left exception -> Left exception
                    Right context' ->
                        -- This one is also a bit tricky, but only because we will not
                        -- be doing checkMultipleSequential. Only one of the
                        -- alternatives ever gets executed, which means that the
                        -- alternatives can move the same variables.
                        -- moveCheckMultiple context' owner alternatives
                        moveCheckMultiple context' (Just expression) alternatives

instance MoveCheck Alternative where
    moveCheck context owner (AlgebraicAlternative constructor variables body) =
        moveCheck context owner body
    moveCheck context owner (DefaultAlternative name body) =
        moveCheck context owner body

instance MoveCheck Atom where
    moveCheck context owner (Variable variable) =
        Right context
        -- moveCheck context owner variable
    moveCheck context owner (Literal _) = Right context
    moveCheck context owner (Borrow _) = todo

-- instance MoveCheck Variable where
--     moveCheck context owner (MovedVariable name) =
--         -- The important part: Check if the variable is already borrowed. We can
--         -- assume that the name resolution step has already been completed and
--         -- therefore we don't need to check whether the variable exists or not.
--         case lookup name context of
--             -- This is fine, the variable has not been moved yet.
--             Nothing ->
--                 case owner of
--                     Nothing -> Right context
--                     Just ownerName -> Right $ (name, ownerName) : context
--             -- There exists a move of the variable in current move context,
--             -- which means that this move is invalid. Throw an exception.
--             Just ownerName ->
--                 -- Left (MoveCheckException $ "Variable '" ++ name ++ "' has already been moved to " ++ pretty ownerName ++ ".")
--                 Left (MoveCheckException $ "Variable '" ++ name ++ "' has already been moved.")

--     moveCheck context owner (BorrowedVariable name) = Right context

instance MoveCheck Binding where
  moveCheck context owner (Binding name value) = moveCheck context owner value

moveCheckProgram :: Program -> Either MoveCheckException MoveCheckContext
moveCheckProgram = moveCheckSequential [] Nothing

moveCheckSequential :: (MoveCheck a) => MoveCheckContext -> Maybe Expression -> [a] -> Either MoveCheckException MoveCheckContext
moveCheckSequential context owner items =
    case items of
        [] -> Right context
        item : rest ->
            case moveCheck context owner item of
                Left exception -> Left exception
                Right context' -> moveCheckSequential context' owner rest

moveCheckMultiple :: (MoveCheck a) => MoveCheckContext -> Maybe Expression -> [a] -> Either MoveCheckException MoveCheckContext
moveCheckMultiple context owner items =
    case items of
        [] -> Right context
        item : rest ->
            case moveCheck context owner item of
                Left exception -> Left exception
                Right context' -> 
                    case moveCheckMultiple context owner rest of
                        Left exception -> Left exception
                        Right context'' -> Right $ join context' context''

-- TODO: Remove duplicates
join context context' =
    -- Set.toList (Set.union (Set.fromList context) (Set.fromList context'))
    context ++ context'