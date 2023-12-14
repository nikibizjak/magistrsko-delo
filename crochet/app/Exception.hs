module Exception where
import Control.Exception ( Exception )

data CompilerException
    = ParserException String
    | InterpreterException String
    deriving (Show)

instance Exception CompilerException