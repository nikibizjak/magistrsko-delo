module Stg.Interpreter.GarbageCollection where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Stg.Interpreter.Types
import Data.Maybe (mapMaybe)
import Stg.Stg
import Stg.Pretty

collectGarbage :: MachineState -> MachineState
collectGarbage state@MachineState {
    machineExpression = expression,
    machineStack = stack,
    machineHeap = heap,
    machineEnvironment = environment,
    machineStep = i
} =
    let
        -- Figure out which addresses are live. An address is live if there
        -- exists a pointer to that address from anywhere in the heap, stack,
        -- environment or expression itself.
        addressesExpression = liveInExpression expression
        addressesEnvironment = liveInEnvironment environment
        addressesStack = liveOnStack stack
        addressesHeap = liveOnHeap heap

        liveAddresses = Set.unions [
            addressesExpression,
            addressesEnvironment,
            addressesStack,
            addressesHeap ]

        -- Remove all addresses that are not live anymore
        heap' = Map.restrictKeys heap liveAddresses
    in
        state {
            machineHeap = heap'
        }

extractAddress :: MemoryValue -> Maybe HeapAddress
extractAddress (MemoryAddress address) = Just address
extractAddress _ = Nothing

liveInExpression :: Expression -> Set.Set HeapAddress
liveInExpression = live

liveInEnvironment :: Environment -> Set.Set HeapAddress
liveInEnvironment environment =
    Set.fromList $ mapMaybe extractAddress (Map.elems environment)

liveOnStack :: Stack -> Set.Set HeapAddress
liveOnStack stack =
    let
        extractAddresses (ApplyContinuation arguments) =
            Set.fromList $ mapMaybe extractAddress arguments
        extractAddresses (UpdateContinuation address) =
            Set.singleton address
        extractAddresses (CaseContinuation _ environment) =
            liveInEnvironment environment
    in
        Set.unions $ map extractAddresses stack

liveOnHeap :: Heap -> Set.Set HeapAddress
liveOnHeap heap =
    let
        extractAddresses (HeapObject _ environment) =
            liveInEnvironment environment
        extractAddresses (Indirection address) =
            Set.singleton address
    in
        Set.unions $ map extractAddresses (Map.elems heap)

class Liveness a where
    live :: a -> Set.Set HeapAddress

liveMany :: Liveness a => [a] -> Set.Set HeapAddress
liveMany items =
    Set.unions $ map live items

instance Liveness Atom where
    live (Literal (Address address)) = Set.singleton (HeapAddress address)
    live _ = Set.empty

instance Liveness Expression where
    live (Atom atom) = live atom
    live (FunctionApplication _ _ arguments) = liveMany arguments
    live (PrimitiveOperation _ arguments) = liveMany arguments
    live (LetIn _ object body) =
        Set.union (live object) (live body)
    live (CaseOf scrutinee alternatives) =
        Set.union (live scrutinee) (liveMany alternatives)

instance Liveness Alternative where
    live (AlgebraicAlternative _ _ body) = live body
    live (DefaultAlternative _ body) = live body

instance Liveness Object where
    live (Function _ body) = live body
    live (PartialApplication _ arguments) = liveMany arguments
    live (Constructor _ arguments) = liveMany arguments
    live (Thunk expression) = live expression
    live BlackHole = Set.empty