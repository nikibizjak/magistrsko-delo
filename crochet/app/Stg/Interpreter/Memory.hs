module Stg.Interpreter.Memory where

import Stg.Interpreter.Types
import Stg.Stg
import Stg.FreeVariables
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace

-- Memory management
allocate :: Heap -> HeapAddress -> HeapObject -> (Heap, HeapAddress)
allocate heap heapPointer@(HeapAddress heapPointerAddress) heapObject =
    let
        heap' = Map.insert heapPointer heapObject heap
        heapPointer' = HeapAddress (heapPointerAddress + 1)
    in
        (heap', heapPointer')

allocateMany :: Heap -> HeapAddress -> Environment -> Program -> (Heap, HeapAddress, Environment)
allocateMany initialHeap initialHeapPointer initialEnvironment =
    foldr (\(Binding name object) (heap, heapPointer, environment) ->
        let
            environment' = Map.insert name (MemoryAddress heapPointer) environment
            (heap', heapPointer') = allocate heap heapPointer (HeapObject object Map.empty)
        in
            (heap', heapPointer', environment')
    ) (initialHeap, initialHeapPointer, initialEnvironment)

initializeTopLevelObjects :: Program -> (Heap, HeapAddress, Environment)
initializeTopLevelObjects bindings =
    let
        topLevelNames = map (\(Binding name object) -> name) bindings
        topLevelEnvironment = Map.fromList [(name, MemoryAddress (HeapAddress 0)) | name <- topLevelNames]

        (finalHeap, finalHeapPointer, finalEnvironment) = foldr (\(Binding name object) (heap, heapPointer, environment) ->
                let
                    environment' = Map.insert name (MemoryAddress heapPointer) environment
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
        Just object@(HeapObject _ _) -> Just object
        Just object@(Indirection _) -> Just object
        -- Just (Indirection nextAddress) ->
        --     -- Follow indirections
        --     heapLookup nextAddress heap

getEnvironmentValue :: Environment -> Atom -> MemoryValue
getEnvironmentValue environment atom =
    case atom of
        Literal (Integer n) -> MemoryInteger n
        Literal (Address address) -> MemoryAddress (HeapAddress address)
        Variable name ->
            -- Get the value of a variable in current environment.
            resolveVariable environment name

resolveVariable :: Environment -> Variable -> MemoryValue
resolveVariable environment name =
    case Map.lookup name environment of
        Just value -> value
        _ -> trace ("Variable '" ++ name ++ "' not in environment.") (MemoryInteger 404)

heapLookupVariable :: Variable -> Environment -> Heap -> Maybe HeapObject
heapLookupVariable variable environment heap =
    case resolveVariable environment variable of
        (MemoryAddress address) -> heapLookup address heap
        _ -> Nothing
