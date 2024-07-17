module Stg.Interpreter.Memory where

import Stg.Interpreter.Types
import Stg.Stg
import Stg.FreeVariables
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Memory management
allocate :: Heap -> HeapAddress -> HeapObject -> (Heap, HeapAddress)
allocate heap heapPointer@(HeapAddress heapPointerAddress) heapObject =
    let
        heap' = Map.insert heapPointer heapObject heap
        heapPointer' = HeapAddress (heapPointerAddress + 1)
    in
        (heap', heapPointer')

allocateMany :: Heap -> HeapAddress -> Map.Map String HeapAddress -> Program -> (Heap, HeapAddress, Map.Map String HeapAddress)
allocateMany initialHeap initialHeapPointer initialEnvironment =
    foldr (\(Binding name object) (heap, heapPointer, environment) ->
        let
            environment' = Map.insert name heapPointer environment
            (heap', heapPointer') = allocate heap heapPointer (HeapObject object Map.empty)
        in
            (heap', heapPointer', environment')
    ) (initialHeap, initialHeapPointer, initialEnvironment)

initializeTopLevelObjects :: Program -> (Heap, HeapAddress, Map.Map String HeapAddress)
initializeTopLevelObjects bindings =
    let
        topLevelNames = map (\(Binding name object) -> name) bindings
        topLevelEnvironment = Map.fromList [(name, HeapAddress 0) | name <- topLevelNames]

        (finalHeap, finalHeapPointer, finalEnvironment) = foldr (\(Binding name object) (heap, heapPointer, environment) ->
                let
                    environment' = Map.insert name heapPointer environment
                    heapObject = HeapObject object Map.empty
                    (heap', heapPointer') = allocate heap heapPointer heapObject
                in
                    (heap', heapPointer', environment')
            ) (Map.empty, HeapAddress 0, topLevelEnvironment) bindings

        finalHeap' = Map.map (\(HeapObject object _) ->
            let
                requiredVariables = freeVariables object
                closureEnvironment = Map.restrictKeys finalEnvironment requiredVariables
            in
                HeapObject object closureEnvironment
            ) finalHeap
    in
        (finalHeap', finalHeapPointer, finalEnvironment)

heapLookup :: HeapAddress -> Heap -> Maybe HeapObject
heapLookup address heap =
    case Map.lookup address heap of
        Nothing -> Nothing
        Just object@(HeapObject a b) -> Just object
        Just (Indirection nextAddress) ->
            -- Follow indirections
            heapLookup nextAddress heap

heapLookupVariable :: Variable -> Map.Map Variable HeapAddress -> Heap -> Maybe HeapObject
heapLookupVariable variable environment = heapLookup (resolveVariable environment variable)

resolveVariable :: Map.Map Variable HeapAddress -> Variable -> HeapAddress
resolveVariable environment variable =
    case Map.lookup variable environment of
        Just address -> address

getAddress :: Environment -> Atom -> HeapAddress
getAddress environment atom =
    case atom of
        Literal (Integer n) -> HeapAddress n
        Literal (Address address) -> HeapAddress address
        Variable name -> resolveVariable environment name