module Stg.Interpreter (
    run,
    runDebug,
    runDebugExpression,
    runDebugAll,
    runDebugWriteAllToFile,
    InterpreterException(..),
    MachineState(..),
) where

import Stg.Interpreter.Internal
import Stg.Interpreter.Evaluation