{-# LANGUAGE OverloadedStrings #-}
module Stg.Interpreter.Debug where

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty
import Stg.Interpreter.Types
import Stg.Pretty
import qualified Data.Map as Map

renderEnvironment environment = do
    H.table ! A.class_ "environment" $ H.tbody $ do
        let environmentItems = Map.toList environment
        forM_ environmentItems (\(name, address) -> H.tr $ do
            H.td $ toHtml name
            H.td $ toHtml $ show address
            )

renderHeap heap = do
    H.h2 "Heap"
    H.table ! A.class_ "heap" $ do

        H.thead $ H.tr $ do
            H.th "Address"
            H.th "Object"
            H.th "Closure"

        H.tbody $ do
            let heapItems = Map.toList heap
            forM_ heapItems (\(address, HeapObject object environment) -> H.tr $ do
                H.td $ toHtml $ show address
                H.td $ toHtml $ pretty object
                H.td $ renderEnvironment environment)

renderStack stack = do
    H.h2 "Stack"
    H.table ! A.class_ "stack" $ do
        H.thead $ H.tr $ H.th "Frame"
        H.tbody $ do
            forM_ stack (\continuation -> H.tr $ do
                H.td $ toHtml $ show continuation)

renderExpression expression = do
    H.h2 "Expression"
    p $ toHtml (pretty expression)

machineStateToHtml MachineState {
    machineExpression = expression,
    machineStack = stack,
    machineHeap = heap,
    machineHeapPointer = heapPointer,
    machineEnvironment = environment
} =
    renderHtml $
    docTypeHtml $ do
        H.head $ do
            H.title "Machine state"
            -- TODO: Add styling
            -- table { border-collapse: collapse; }
            -- .heap { border: 1px solid black; }
            -- .environment { border: 1px solid red; }
            -- H.style ""
        body $ do
            renderExpression expression
            renderStack stack
            renderHeap heap
            H.hr