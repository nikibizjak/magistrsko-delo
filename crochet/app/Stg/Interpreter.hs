module Stg.Interpreter where

import Control.Exception hiding (evaluate)
import Data.List (find)
import Data.Maybe (isJust)
import Stg.Stg (Expression (LiteralExpression))
import qualified Stg.Stg as Stg
import Data.Char (chr)

data Value
  = Address Int
  | Integer Int
  deriving (Show)

type Environment = [(Stg.Variable, Value)]

data Closure
  = Closure Stg.LambdaForm [Value]
  deriving (Show)

data Code
  = Evaluate Stg.Expression Environment
  | Enter Value
  | ReturnConstructor Stg.Constructor [Value]
  | ReturnInteger Int
  deriving (Show)

val :: Environment -> Environment -> Stg.Atom -> Maybe Value
val local global atom =
  case atom of
    Stg.V name ->
      case lookup name local of
        Just value -> Just value
        Nothing -> lookup name global
    Stg.L k -> Just (Integer k)

valIn :: Environment -> Stg.Variable -> Maybe Value
valIn environment name = lookup name environment

valsIn :: Environment -> [Stg.Variable] -> [Value]
valsIn environment names =
  case names of
    [] -> []
    x : xs -> case valIn environment x of
      Nothing -> valsIn environment xs
      Just value -> value : valsIn environment xs

vals :: Environment -> Environment -> [Stg.Atom] -> [Value]
vals local global atoms =
  case atoms of
    [] -> []
    x : xs -> case val local global x of
      Nothing -> vals local global xs
      Just value -> value : vals local global xs

pop = splitAt

remove key environment =
  case environment of
    [] -> []
    (k, v) : xs ->
      if k == key
        then xs
        else (k, v) : remove key xs

allocate :: [(Int, Closure)] -> Int
allocate heap =
  foldl max 0 (map fst heap) + 1

single n = chr (97 + mod n 26)

multiple n =
  let
    (a, b) = divMod n 26
    rest = if a == 0 then [] else multiple (a - 1)
  in reverse (single b : rest)

generateVariables n = map multiple [0..n]

newtype InterpreterException = RuntimeException String deriving (Show)

instance Exception InterpreterException

-- step :: Code -> [Value] -> p3 -> p4 -> [(Int, Closure)] -> Environment -> Either InterpreterException (Code, [Value], p3, p4, p5, Environment)
step code argumentStack returnStack updateStack heap globals =
  case code of
    -- Applications
    Evaluate (Stg.Application f xs) locals ->
      case val locals globals (Stg.V f) of
        Just (Address a) ->
          Right (Enter (Address a), vals locals globals xs ++ argumentStack, returnStack, updateStack, heap, globals, True)
        Just (Integer k) ->
          Right (ReturnInteger k, argumentStack, returnStack, updateStack, heap, globals, True)
        -- Since the f must be a value, it it always either a primitive (an
        -- Integer) or an Address of a closure. Therefore this case should never
        -- happen unless we are trying to add new Value.
        _ -> Left (RuntimeException ("Variable " ++ show f ++ " doesn't evaluate to an address or an integer"))
    -- Entering a closure
    Enter (Address a) ->
      -- Find closure in heap
      case lookup a heap of
        Nothing -> Left (RuntimeException ("No closure at address " ++ show a))
        Just (Closure (Stg.Lambda vs Stg.Updatable [] e) wsf) ->
          -- Update rule 15
          let
            locals = zip vs wsf
            updateStack' = (argumentStack, returnStack, a) : updateStack
            heap' = remove a heap
          in
            Right (Evaluate e locals, [], [], updateStack', heap', globals, True)
        Just (Closure (Stg.Lambda vs Stg.NotUpdatable xs e) wsf) ->
          -- Check if there is enough arguments on the argument stack
          if length argumentStack < length xs
            then
              if not (null returnStack)
              then Left (RuntimeException ("Not enough arguments on argument stack. Required " ++ show (length xs) ++ " arguments, received only " ++ show (length argumentStack)))
              else
                let
                  (argumentStack', returnStack', updateClosureAddress) : updateStack' = updateStack
                  argumentStack'' = argumentStack ++ argumentStack'
                  (xs1, xs2) = pop (length argumentStack') xs
                  
                  lambda = Stg.Lambda (vs ++ xs1) Stg.NotUpdatable xs2 e
                  closure = Closure lambda (wsf ++ argumentStack) 

                  heap' = (updateClosureAddress, closure) : heap
                in
                  Right (Enter (Address a), argumentStack'', returnStack', updateStack', heap', globals, True)
            else
              let (wsa, argumentStack') = pop (length xs) argumentStack
                  heap' = remove a heap
                  locals = zip vs wsf ++ zip xs wsa
               in Right (Evaluate e locals, argumentStack', returnStack, updateStack, heap', globals, True)
    -- Evaluate let expression - create a new closure on the heap
    Evaluate (Stg.LetIn bindings e) locals ->
      let variableNames = map fst bindings
          lambdaForms = map snd bindings
          lastAddress = allocate heap
          addresses = map (+ lastAddress) [0 .. (length bindings)]
          locals' = zip variableNames (map Address addresses) ++ locals
          freeVariables = map Stg.freeVariables lambdaForms
          freeVaraibleValues = map (valsIn locals) freeVariables
          closures = zipWith Closure lambdaForms freeVaraibleValues
          heap' = zip addresses closures ++ heap
       in Right (Evaluate e locals', argumentStack, returnStack, updateStack, heap', globals, True)
    -- Evaluate let **rec** expression - create a new closure on the heap
    Evaluate (Stg.LetRec bindings e) locals ->
      let variableNames = map fst bindings
          lambdaForms = map snd bindings
          lastAddress = allocate heap
          addresses = map (+ lastAddress) [0 .. (length bindings)]
          locals' = zip variableNames (map Address addresses) ++ locals
          freeVariables = map Stg.freeVariables lambdaForms
          freeVaraibleValues = map (valsIn locals') freeVariables
          closures = zipWith Closure lambdaForms freeVaraibleValues
          heap' = zip addresses closures ++ heap
       in Right (Evaluate e locals', argumentStack, returnStack, updateStack, heap', globals, True)
    -- Case expression
    Evaluate (Stg.CaseOf e alternatives) locals ->
      Right (Evaluate e locals, argumentStack, (alternatives, locals) : returnStack, updateStack, heap, globals, True)
    Evaluate (Stg.ConstructorExpression c xs) locals ->
      Right (ReturnConstructor c (vals locals globals xs), argumentStack, returnStack, updateStack, heap, globals, True)
    
    ReturnConstructor c ws ->

      case (argumentStack, returnStack) of
        
        -- Update rule 16: The argument and return stacks are empty.
        ([], []) ->
          -- This is an update
          let
            -- Pop a continuation from the update stack
            (argumentStack', returnStack', updateClosureAddress) : updateStack' = updateStack
            -- TODO: The error is happening here!

            -- Generate a sequence of arbitary length(ws) distinct variables.
            vs = generateVariables (length ws)

            lambda = Stg.Lambda vs Stg.NotUpdatable [] (Stg.ConstructorExpression c (map Stg.V vs))
            closure = Closure lambda ws
            heap' = (updateClosureAddress, closure) : heap
          in
            Right (ReturnConstructor c ws, argumentStack', returnStack', updateStack', heap', globals, True)
        
        -- The return stack has a continuation!
        (_, (alternatives, locals) : returnStack') ->
          let
            foundAlternative = find (\(Stg.AlgebraicAlternative constructor _ _) -> constructor == c) alternatives
          in case foundAlternative of
            Nothing ->
              -- No algebraic alternative matches the constructor. We now have
              -- two options:
              --   * DefaultAlternative - the easy one, just evaluate its body
              --   * NamedDefaultAlternative - the more complicated one that
              --     allocates a new closure on the heap.
              let
                foundDefaultAlternative = find Stg.isDefaultAlternative alternatives
              in
                case foundDefaultAlternative of
                  Just (Stg.DefaultAlternative e) ->
                    -- The easier version, simply evaluate the body of the alternative
                    Right (Evaluate e locals, argumentStack, returnStack', updateStack, heap, globals, True)
                  Just (Stg.NamedDefaultAlternative name e) ->
                    -- The slightly more complicated version. Here we need to
                    -- heap allocate a new constructor closure to which we bind
                    -- the name.
                    let
                      address = allocate heap
                      vs = generateVariables (length ws)
                      lambda = Stg.Lambda vs Stg.NotUpdatable [] (Stg.ConstructorExpression c (map Stg.V vs))
                      closure = Closure lambda ws
                      heap' = (address, closure) : heap
                      locals' = (name, Address address) : locals
                    in
                      Right (Evaluate e locals', argumentStack, returnStack', updateStack, heap', globals, True)
                  _ -> Left (RuntimeException "No constructor matches and no default alternative.")
            Just (Stg.AlgebraicAlternative c vs e) ->
              let locals' = locals ++ zip vs ws
              in Right (Evaluate e locals', argumentStack, returnStack', updateStack, heap, globals, True)
    -- Built-in operations
    Evaluate (Stg.LiteralExpression k) locals ->
      Right (ReturnInteger k, argumentStack, returnStack, updateStack, heap, globals, True)
    ReturnInteger k ->
      case returnStack of
        -- [] -> Left (RuntimeException "No continuation found.") -- This is the end?
        [] -> Right (ReturnInteger k, argumentStack, returnStack, updateStack, heap, globals, False)
        (alternatives, locals) : returnStack' ->
          let foundAlternative = find (\(Stg.PrimitiveAlternative k' _) -> k' == k) alternatives
          in case foundAlternative of
                Just (Stg.PrimitiveAlternative k e) ->
                  -- The simplest version, there is a continuation and a
                  -- primitive alternative with the value of k. Evaluate the
                  -- body of the alternative, no renaming is needed.
                  Right (Evaluate e locals, argumentStack, returnStack', updateStack, heap, globals, True)
                Nothing ->
                  -- No matching primitive alternative was found. Find a default
                  -- alternative.
                  let
                    foundDefaultAlternative = find Stg.isDefaultAlternative alternatives
                  in
                    case foundDefaultAlternative of
                      Just (Stg.NamedDefaultAlternative name e) ->
                        let locals' = (name, Integer k) : locals
                        in Right (Evaluate e locals', argumentStack, returnStack', updateStack, heap, globals, True)
                      Just (Stg.DefaultAlternative e) ->
                        Right (Evaluate e locals, argumentStack, returnStack', updateStack, heap, globals, True)
                      _ -> Left (RuntimeException "No primitive alternative matches and no default alternative.")

    -- Built-in arithmetic operations
    Evaluate (Stg.BuiltInOperation operator [x1, x2]) locals ->
      case (val locals [] x1, val locals [] x2) of
        (Just (Integer i1), Just (Integer i2)) ->
          -- Actually perform the operation
          let result = case operator of
                Stg.Addition -> i1 + i2
                Stg.Subtraction -> i1 - i2
                Stg.Multiplication -> i1 * i2
                Stg.Division -> div i1 i2
           in Right (ReturnInteger result, argumentStack, returnStack, updateStack, heap, globals, True)
        _ -> Left (RuntimeException "Invalid binary operation types")

    -- If no transition rule matches current state, the STG machine should halt.
    _ -> Right (code, argumentStack, returnStack, updateStack, heap, globals, False)

evaluate :: Code -> [Value] -> [([Stg.Alternative], Environment)] -> [([Value], [([Stg.Alternative], Environment)], Int)] -> [(Int, Closure)] -> Environment -> Either      InterpreterException      (Code, [Value], [([Stg.Alternative], Environment)],       [([Value], [([Stg.Alternative], Environment)], Int)],       [(Int, Closure)], Environment)
evaluate code argumentStack returnStack updateStack heap globals =
  case step code argumentStack returnStack updateStack heap globals of
    Left error -> Left error
    -- The last Boolean flag tells us if there is some other transition that can be done. If no transition rules
    -- match the current state, the [step] function will set the flag to [False] to indicate that no further action
    -- can be taken.
    Right (code', argumentStack', returnStack', updateStack', heap', globals', True) ->
      evaluate code' argumentStack' returnStack' updateStack' heap' globals'
    Right (code', argumentStack', returnStack', updateStack', heap', globals', False) ->
      Right (code', argumentStack', returnStack', updateStack', heap', globals')

-- main :: IO ()
-- main =
--   let code = Stg.LetIn [
--         ("map", Stg.Lambda [] False ["f", "xs"]
--             (Stg.CaseOf (Stg.Application "xs" [])
--                 [
--                     Stg.AlgebraicAlternative "Nil" [] (Stg.ConstructorExpression "Nil" []),
--                     Stg.AlgebraicAlternative "Cons" ["y", "ys"] (
--                         (Stg.LetIn )
--                             )]))] (Stg.LiteralExpression 19)
--    in case evaluate (Evaluate code []) [] [] [] [] [] of
--         Left (RuntimeException exception) -> putStrLn exception
--         Right (code', argumentStack', returnStack', updateStack', heap', globals') ->
--           do
--             print code'
--             print argumentStack'
--             print returnStack'
--             -- print updateStack'
--             print heap'
--             print globals'