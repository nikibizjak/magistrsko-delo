module Stg.BorrowCheck where
import Stg.Stg

newtype BorrowCheckException = BorrowCheckException String

class BorrowCheck a where
  borrowCheck :: a -> Either BorrowCheckException ()

todo = Left (BorrowCheckException "TODO")

instance BorrowCheck Object where
    borrowCheck (Function parameters body) = todo
    borrowCheck (PartialApplication function arguments) = todo
    borrowCheck (Constructor name arguments) = todo
    borrowCheck (Thunk expression) = todo
    borrowCheck BlackHole = todo

instance BorrowCheck Expression where
    borrowCheck (Atom atom) = todo
    borrowCheck (FunctionApplication function arity arguments) = todo
    borrowCheck (PrimitiveOperation operation arguments) = todo
    borrowCheck (LetIn name value body) = todo
    borrowCheck (LetRec definitions body) = todo
    borrowCheck (CaseOf scrutinee alternatives) = todo

instance BorrowCheck Alternative where
    borrowCheck (AlgebraicAlternative constructor variables body) = todo
    borrowCheck (DefaultAlternative name body) = todo

instance BorrowCheck Atom where
    borrowCheck (Variable variable) = todo
    borrowCheck (Literal _) = todo

instance BorrowCheck Variable where
    borrowCheck (MovedVariable name) = todo
    borrowCheck (BorrowedVariable name) = todo

instance BorrowCheck Binding where
  borrowCheck (Binding name value) = todo

borrowCheckSequential :: (BorrowCheck a) => [a] -> Either BorrowCheckException ()
borrowCheckSequential items =
    case items of
        [] -> Right ()
        item : rest ->
            case borrowCheck item of
                Left exception -> Left exception
                Right context' -> borrowCheckSequential rest

borrowCheckProgram :: Program -> Either BorrowCheckException ()
borrowCheckProgram = borrowCheckSequential