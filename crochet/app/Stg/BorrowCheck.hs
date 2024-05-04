module Stg.BorrowCheck where
import Stg.Stg
import Control.Monad.State
import Control.Monad (foldM)
import Debug.Trace
import Stg.Pretty
import qualified Data.Graph as Graph
import qualified Stg.NameResolution as NameResolution

-- Define exception for borrow checking
newtype BorrowCheckException
    = BorrowCheckException String
    deriving (Show)

throw text = return $ Left $ BorrowCheckException text

-- Lifetimes
newtype Lifetime
    = Lifetime Int
    deriving (Eq, Ord)

staticLifetime = Lifetime 0

instance Show Lifetime where
  show (Lifetime 0) = "'static"
  show (Lifetime n) = '\'' : show n

-- Borrow checking context - a mapping between variable name and its lifetime.
type Context = [(String, Lifetime)]

data Equation
    = Outlives Lifetime Lifetime

instance Show Equation where
    show (Outlives a b) = show a ++ " < " ++ show b

-- Generate a new unique lifetime that hasn't been used anywhere yet.
fresh :: State Int Lifetime
fresh = do
    n <- get
    put (n + 1)
    return (Lifetime n)

class BorrowCheck a where
  borrowCheck :: Context -> a -> State Int (Either BorrowCheckException (Lifetime, [Equation]))

todo = do
    return $ Left (BorrowCheckException "Not implemented yet")

instance BorrowCheck Object where
    borrowCheck context (Function parameters body) = do
        -- let f = FUN( p_1, ..., p_n -> body )
        -- in  expression

        -- For each of the parameters we only know if it has been borrowed or
        -- moved. The programmer must mark each parameter that is borrowed with
        -- an ampersand and a named lifetime identifier (&'lifetime T). We don't
        -- actually know the data type since type inference is done *before*
        -- conversion to the STG language.

        -- A function FUN(&x &y -> x) has a general type of: &'a T -> &'b T -> &'a T
        todo
    borrowCheck context (PartialApplication function arguments) = todo
    borrowCheck context (Constructor name arguments) = todo

    borrowCheck context (Thunk expression) = do
        lifetime <- fresh
        result <- borrowCheck context expression
        case result of
            Left exception -> return $ Left exception
            Right (expressionLifetime, equations) ->
                return $ Right (lifetime, Outlives lifetime expressionLifetime : equations)

    borrowCheck context BlackHole = do
        lifetime <- fresh
        return $ Right (lifetime, [])

instance BorrowCheck Expression where
    borrowCheck context (Atom atom) = borrowCheck context atom
    borrowCheck context (FunctionApplication function arity arguments) = todo

    borrowCheck context (PrimitiveOperation operation arguments) = todo

    borrowCheck context (LetIn name value body) = do
        -- Lifetime for the entire let expression
        lifetimeLet <- fresh

        case value of

            Thunk expression -> do

                -- Lifetime for the bound name
                lifetimeDefinition <- fresh
                let context' = (name, lifetimeDefinition) : context

                resultBody <- borrowCheck context' body
                case resultBody of
                    Left exception -> return $ Left exception
                    Right (lifetimeBody, equationsBody) ->
                        return $ Right (lifetimeLet, Outlives lifetimeBody lifetimeLet : Outlives lifetimeLet lifetimeDefinition : equationsBody)

            Function _ _ -> do

                resultValue <- borrowCheck context value
                case resultValue of
                    Left exception -> return $ Left exception
                    Right (lifetimeValue, equationsValue) -> do

                        let context' = (name, lifetimeValue) : context

                        resultBody <- borrowCheck context' body
                        case resultBody of
                            Left exception -> return $ Left exception
                            Right (lifetimeBody, equationsBody) ->
                                return $ Right (lifetimeLet, Outlives lifetimeBody lifetimeLet : Outlives lifetimeLet lifetimeValue : (equationsValue ++ equationsBody))

            PartialApplication function arguments -> todo
            Constructor name arguments -> todo
            BlackHole -> todo

    borrowCheck context (LetRec definitions body) = todo
    borrowCheck context (CaseOf scrutinee alternatives) = todo

instance BorrowCheck Alternative where
    borrowCheck context (AlgebraicAlternative constructor variables body) = todo
    borrowCheck context (DefaultAlternative name body) = todo

instance BorrowCheck Atom where
    borrowCheck context (Variable variable) =
        borrowCheck context variable
    borrowCheck context (Literal _) = do
        lifetime <- fresh
        return $ Right (lifetime, [])

instance BorrowCheck Variable where
    borrowCheck context (MovedVariable name) = todo
    borrowCheck context (BorrowedVariable name) =
        case lookup name context of
            Nothing -> throw $ "Borrowed variable '" ++ name ++ "' not in context."
            Just lifetime -> return $ Right (lifetime, [])

instance BorrowCheck Binding where
  borrowCheck context (Binding name value) = do
    result <- borrowCheck ((name, staticLifetime) : context) value
    case result of
        Left exception -> return $ Left exception
        Right (lifetime, equations) ->
            return $ Right (lifetime, Outlives staticLifetime lifetime : equations)

-- borrowCheckSequential :: (BorrowCheck a) => Context -> [a] -> State Int (Either BorrowCheckException ([Lifetime], [Equation]))
borrowCheckSequential context items =
    case items of
        [] -> return $ Right ([], [])
        item : rest -> do
            result <- borrowCheck context item
            case result of
                Left exception -> return $ Left exception
                Right (lifetime, equations) -> do
                    restResult <- borrowCheckSequential context rest
                    case restResult of
                        Left exception -> return $ Left exception
                        Right (restLifetimes, restEquations) ->
                            return $ Right (lifetime : restLifetimes, equations ++ restEquations)

-- borrowCheckProgram :: Program -> Either BorrowCheckException ()
borrowCheckProgram program =
    let
        -- Prepare the context. All toplevel names should have a lifetime of
        -- 'static (or in our case StaticLifetime).
        names = map (\(Binding name _) -> name) program
        context = map (\name -> (name, staticLifetime)) names

        -- Actually do the real borrow checking
        borrowCheckStateful = borrowCheckSequential context program
        result = evalState borrowCheckStateful 1
    in
        case result of
            Left exception -> Left exception
            Right (lifetimes, equations) ->
                if hasCycles equations
                then Left (BorrowCheckException "Graph contains cycles!")
                else Right (lifetimes, equations)

borrowCheckItem :: BorrowCheck a => a -> Either BorrowCheckException (Lifetime, [Equation])
borrowCheckItem a =
    let
        borrowCheckStateful = borrowCheck [] a
        result = evalState borrowCheckStateful 1
    in
        case result of
            Left exception -> Left exception
            Right (lifetimes, equations) ->
                if hasCycles equations
                then Left (BorrowCheckException "Graph contains cycles!")
                else result

extractLifetimeEquation (Outlives (Lifetime first) (Lifetime second)) =
    [first, second]

graphBounds :: Foldable t => t Equation -> (Int, Int)
graphBounds equations =
    let
        allNodes = foldr (\x acc -> extractLifetimeEquation x ++ acc) [] equations
    in
        (minimum allNodes, maximum allNodes)

constructGraph equations =
    let
        bounds = graphBounds equations
        edges = map (\(Outlives (Lifetime u) (Lifetime v)) -> (u, v)) equations
    in
        Graph.buildG bounds edges

hasCycles equations =
    let
        graph = constructGraph equations
    in
        length (Graph.vertices graph) /= length (Graph.scc graph)