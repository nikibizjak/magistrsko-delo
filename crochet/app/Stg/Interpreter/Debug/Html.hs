{-# LANGUAGE OverloadedStrings #-}
module Stg.Interpreter.Debug.Html (debugHtml) where

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty
import Stg.Interpreter.Types
import Stg.Pretty
import qualified Data.Map as Map
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

writeToFileInDirectory :: FilePath -> FilePath -> String -> IO ()
writeToFileInDirectory directory filename content = do
    -- Create the directory if it doesn't yet exist.
    createDirectoryIfMissing True directory
    -- Construct the full file path.
    let filePath = directory </> filename
    -- Write content to the file
    writeFile filePath content

filenameFromStep :: Int -> FilePath
filenameFromStep step = "step" ++ show step ++ ".html"

filenameFromMachineState :: MachineState -> FilePath
filenameFromMachineState state@MachineState {
    machineStep = i
} = filenameFromStep i
    
debugHtml :: FilePath -> MachineState -> IO MachineState
debugHtml directory state@MachineState {
    machineStep = i
} = do
    let html = machineStateToHtml state
    let filename = filenameFromMachineState state
    writeToFileInDirectory directory filename html
    return state

machineStateToHtml MachineState {
    machineExpression = expression,
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment,
    machineStep = step
} =
    renderHtml $
    docTypeHtml $ do
        H.head $ do
            H.title $ toHtml $ "Machine state " ++ show step
            -- TODO: Add styling
            -- table { border-collapse: collapse; }
            -- .heap { border: 1px solid black; }
            -- .environment { border: 1px solid red; }
            -- H.style ""
        body $ do
            H.h1 $ toHtml $ "Machine state " ++ show step
            
            -- Links to previous and next states
            let previous = filenameFromStep (step - 1)
            let next = filenameFromStep (step + 1)
            H.a ! A.href (toValue previous) $ "Previous"
            H.a ! A.href (toValue next) $ "Next"

            renderExpression expression
            H.h2 "Environment"
            renderEnvironment environment
            renderStack stack
            renderHeap heap

renderEnvironment environment = do
    H.table ! A.class_ "environment" $ H.tbody $ do
        let environmentItems = Map.toList environment
        forM_ environmentItems (\(name, address) -> H.tr $ do
            H.td $ toHtml name
            H.td $ code $ toHtml $ show address
            )

renderHeapObject (address, object) =
    H.tr $ do
    case object of
        HeapObject object environment -> do
            H.td $ code $ toHtml $ show address
            H.td $ code $ toHtml $ pretty object
            H.td $ renderEnvironment environment
        Indirection otherAddress -> do
            H.td $ code $ toHtml $ show address
            H.td $ code $ toHtml ("INDIRECTION " ++ show otherAddress)
            H.td ""

renderHeap heap = do
    H.h2 "Heap"
    H.table ! A.class_ "heap" $ do

        H.thead $ H.tr $ do
            H.th "Address"
            H.th "Object"
            H.th "Closure"

        H.tbody $ do
            let heapItems = Map.toList heap
            forM_ heapItems renderHeapObject

renderStack stack = do
    H.h2 "Stack"
    H.table ! A.class_ "stack" $ do
        H.thead $ H.tr $ H.th "Frame"
        H.tbody $ do
            forM_ stack (\continuation -> H.tr $ do
                H.td $ code $ toHtml $ show continuation)

renderExpression expression = do
    H.h2 "Expression"
    code $ toHtml (pretty expression)