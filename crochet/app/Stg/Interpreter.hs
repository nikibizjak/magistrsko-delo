module Stg.Interpreter (
    run,
    runDebug,
    runDebugExpression,
    runDebugAll,
    InterpreterException(..)
) where

import Stg.Interpreter.Internal
import Stg.Interpreter.Evaluation