module Exception where
import Control.Exception ( Exception )

data CompilerException
    = ParserException String
    | TypeCheckException String
    | InterpreterException String
    | NotImplementedException
    deriving (Show)

instance Exception CompilerException