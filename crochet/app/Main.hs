module Main where

import Stg.Stg
import Stg.Pretty
import Stg.Ownership
import Stg.FreeVariables
import Stg.NameResolution
import Stg.MoveCheck
import Stg.BorrowCheck

program :: Program
program = [
    -- Binding "main" (Thunk (Atom $ Literal $ Integer 1))
    -- Binding "main" (Thunk (Atom $ Variable $ MovedVariable "main"))
    -- Binding "main" (Thunk (LetIn "a" (Constructor "True" []) (Atom $ Variable $ MovedVariable "a")))
    -- Binding "main" (Thunk $
    --     LetIn "a" (Constructor "I" [Literal $ Integer 10]) $
    --     LetIn "b" (Constructor "Just" [Variable $ MovedVariable "a"]) $
    --         Atom (Literal $ Integer 1)
    --     )
    -- Binding "main" (Thunk $
    --     LetIn "a" (Constructor "I" [Literal $ Integer 10]) $
    --     Atom (Literal $ Integer 1)
    --     )
    -- Binding "z" BlackHole,

    -- Prvi delujoci primer?
    -- Binding "f" (Constructor "True" []),
    -- Binding "main" (Thunk $
    --     LetIn "a" (Constructor "I" [Literal $ Integer 10]) $
    --     LetIn "b" (Constructor "True" []) $
    --     -- LetIn "c" (Thunk $ Atom (Variable $ MovedVariable "a")) $
    --     LetIn "c" (Thunk $ FunctionApplication (BorrowedVariable "f") Unknown [Variable $ MovedVariable "b"]) $
    --     -- LetIn "c" (Thunk $ FunctionApplication (MovedVariable "f") Unknown [Variable $ MovedVariable "b"]) $
    --     -- LetIn "d" (Thunk $ Atom (Variable $ MovedVariable "a")) $
    --     Atom (Variable $ MovedVariable "a")
    -- )

    -- Ne sme delovati, ker je main toplevel (staticen)
    -- Binding "main" (Thunk $ Atom $ Variable $ MovedVariable "x")

    -- Binding "main" (Thunk $
    --     LetIn "a" (Thunk $ LetIn "b" (Constructor "True" []) (Atom (Variable $ MovedVariable "b"))) $
    --     Atom (Variable $ MovedVariable "a")
    -- )

    -- Binding "main" (Thunk $
    --     LetIn "a" (Constructor "I" [Literal $ Integer 10]) $
    --     LetIn "result" (Thunk $
    --     CaseOf (Atom $ Variable $ MovedVariable "a") [
    --         AlgebraicAlternative "True" [] (Atom (Variable $ MovedVariable "a")),
    --         DefaultAlternative (MovedVariable "x") (Atom (Variable $ MovedVariable "x"))
    --     ]) $
    --     Atom (Variable $ MovedVariable "result")
    -- )

    -- Binding "main" (Thunk $
    --     LetIn "f" (Function [MovedVariable "x", MovedVariable "y"] (Atom $ Literal $ Integer 13)) $
    --     LetIn "a" (Constructor "Integer" [Literal $ Integer 2]) $
    --     LetIn "b" (Constructor "Integer" [Literal $ Integer 3]) $
    --     FunctionApplication (MovedVariable "f") Unknown [
    --         -- Ne deluje, ce sta oba referenci - v tem primeru bi moral tudi
    --         -- stars prevzeti odgovornost (lastnistvo)
    --         Variable $ MovedVariable "a",
    --         Variable $ MovedVariable "b"
    --     ]
    -- )

    -- Binding "main" (Thunk $
    --     LetIn "a" (Constructor "Integer" [Literal $ Integer 3]) $
    --     -- LetIn "b" (Constructor "Pair" [Variable $ MovedVariable "a", Variable $ MovedVariable "a"]) $
    --     LetIn "b" (Thunk $ Atom $ Variable $ MovedVariable "a") $
    --     LetIn "c" (Thunk $ Atom $ Variable $ MovedVariable "a") $
    --     Atom (Variable $ MovedVariable "b")
    -- )
    -- Binding "main" (Thunk $
    --     LetRec [
    --         ("b", Constructor "Pair" [Variable $ MovedVariable "a"]),
    --         ("a", Constructor "Integer" [Literal $ Integer 3])] $
    --     Atom (Variable $ MovedVariable "b")
    -- )

    -- Binding "x" (Thunk $ Atom $ Literal $ Integer 1),
    -- Binding "main" (Thunk $
    --     LetIn "a" (Thunk $ Atom $ Literal $ Integer 0) $
    --     LetIn "b" (Thunk $ Atom $ Literal $ Integer 1) $
    --     LetIn "x" (Constructor "Nil" []) $
    --     LetIn "result" (Thunk $
    --     CaseOf (Atom $ Variable $ MovedVariable "x") [
    --         AlgebraicAlternative "Cons" [] (Atom $ Variable $ MovedVariable "a"),
    --         AlgebraicAlternative "Nil" [] (Atom $ Variable $ MovedVariable "b")
    --     ]) (
    --         LetIn "m" (Thunk $ Atom $ Variable $ MovedVariable "result") $
    --         -- LetIn "n" (Thunk $ Atom $ Variable $ MovedVariable "x") $
    --         Atom (Variable $ MovedVariable "m")
    --     )
    -- )

    Binding "main" (Thunk $
        LetIn "a" (Thunk $ Atom $ Literal $ Integer 0) $
        LetIn "b" (Thunk $ Atom $ Literal $ Integer 1) $
        -- Atom (Variable $ BorrowedVariable "c")
        Atom $ Literal $ Integer 2
    )
    ]

-- main :: IO ()
-- main = do
--     -- Print the program
--     print $ map pretty program

--     -- Perform name resolution - find undefined variables.
--     case nameResolutionProgram program of
--         Left (NameResolutionException exception) ->
--             putStrLn $ "[ ] Name resolution exception: " ++ exception
--         Right _ -> do
--             putStrLn "[x] Name resolution"

--             -- Perform move checking - find invalid moves
--             case moveCheckProgram program of
--                 Left (MoveCheckException exception) ->
--                     putStrLn $ "[ ] Move check exception: " ++ exception
--                 Right _ -> do

--                     putStrLn "[x] Move check"

--                     case borrowCheckProgram program of
--                         Left (BorrowCheckException exception) ->
--                             putStrLn $ "[ ] Borrow check exception: " ++ exception
--                         Right (lifetimes, equations) -> do
--                             putStrLn "[x] Borrow check"
--                             print lifetimes
--                             print equations

main = do
    -- print $ borrowCheckItem (LetIn "a" (Thunk $ Atom $ Literal $ Integer 0) $ LetIn "b" (Thunk $ Atom $ Literal $ Integer 1) $ Atom $ Variable $ BorrowedVariable "b")
    print $ borrowCheckItem (LetIn "a" (Thunk $ Atom $ Literal $ Integer 0) $ LetIn "b" (Thunk $ Atom $ Literal $ Integer 1) $ Atom $ Literal $ Integer 2)
    -- print $ borrowCheckItem (LetIn "a" (Thunk $ Atom $ Literal $ Integer 0) $ LetIn "b" (Thunk $ Atom $ Literal $ Integer 1) $ FunctionApplication (BorrowedVariable "f") Unknown [])
    -- print $ borrowCheckItem (LetIn "f" (Function [BorrowedVariable "x", BorrowedVariable "y"] (Atom $ Variable $ BorrowedVariable "x")) $ Atom $ Literal $ Integer 2)
    