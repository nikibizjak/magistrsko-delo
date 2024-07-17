module Stg.ArityInference where

import Stg.Stg
import qualified Data.Map as Map
import Data.Maybe

type ArityInferenceContext = Map.Map String FunctionArity

-- Get all toplevel definitions

class ArityInference a where
  inferArity :: ArityInferenceContext -> a -> a

inferArityMany :: ArityInference a => ArityInferenceContext -> [a] -> [a]
inferArityMany context = map (inferArity context)

instance ArityInference Atom where
    inferArity context (Variable variable) = Variable variable
    inferArity context (Literal literal) = Literal literal

instance ArityInference Expression where
    inferArity context (Atom atom) =
        Atom (inferArity context atom)
    
    inferArity context (FunctionApplication function _ arguments) =
        case lookupArity function context of
            Just arity -> FunctionApplication function arity arguments
            Nothing -> FunctionApplication function Unknown arguments
    
    inferArity context (PrimitiveOperation operation arguments) =
        PrimitiveOperation operation (inferArityMany context arguments)
    
    inferArity context (CaseOf scrutinee alternatives) =
        CaseOf (inferArity context scrutinee) (inferArityMany context alternatives)

    inferArity context (LetIn name object body) =
        case object of
            Function parameters _ ->
                let
                    arity = Known (length parameters)
                    context' = Map.insert name arity context
                in
                    LetIn name (inferArity context' object) (inferArity context' body)
            _ -> 
                LetIn name (inferArity context object) (inferArity context body)

instance ArityInference Alternative where
    inferArity context (AlgebraicAlternative name parameters body) =
        AlgebraicAlternative name parameters (inferArity context body)
    inferArity context (DefaultAlternative parameter body) =
        DefaultAlternative parameter (inferArity context body)

instance ArityInference Object where
    inferArity context (Function variables expression) =
        Function variables (inferArity context expression)
    inferArity context (PartialApplication function arguments) =
        PartialApplication function (inferArityMany context arguments)
    inferArity context (Constructor constructor arguments) =
        Constructor constructor (inferArityMany context arguments)
    inferArity context (Thunk expression) =
        Thunk (inferArity context expression)
    inferArity context BlackHole =
        BlackHole

instance ArityInference Binding where
    inferArity context (Binding name value) =
        Binding name (inferArity context value)

lookupArity = Map.lookup

inferTopLevelBindingArity :: Binding -> Maybe (String, FunctionArity)
inferTopLevelBindingArity (Binding name value) =
    case value of
        Function parameters _ -> Just (name, Known (length parameters))
        _ -> Nothing

inferArityProgram :: Program -> Program
inferArityProgram program =
    -- Construct a new context of toplevel definitions
    let
        topLevelArities = mapMaybe inferTopLevelBindingArity program
        topLevelContext = Map.fromList topLevelArities
    in
        map (inferArity topLevelContext) program