module Stg.Interpreter (
    run,
    runDebug,
    runDebugExpression,
    runDebugAll,
    InterpreterException(..),
    MachineState(..),
) where

import Stg.Interpreter.Internal
import Stg.Interpreter.Evaluation