module Stg.Interpreter (
    run,
    noDebug,
    debugHtml,
    debugPrint,
    InterpreterException(..),
    MachineState(..),
) where

import Stg.Interpreter.Types
import Stg.Interpreter.Internal
import Stg.Interpreter.Evaluation
import Stg.Interpreter.Debug.Html
import Stg.Interpreter.Debug.Print