module Main where

import Stg.Stg
import Stg.Pretty
import Stg.Ownership
import Stg.FreeVariables
import Stg.NameResolution

program :: Program
program = [
    -- Binding "main" (Thunk (Atom $ Literal $ Integer 1))
    -- Binding "main" (Thunk (Atom $ Variable $ BorrowedVariable "main"))
    -- Binding "main" (Thunk (LetIn "a" (Constructor "True" []) (Atom $ Variable $ BorrowedVariable "a")))
    -- Binding "main" (Thunk $
    --     LetIn "a" (Constructor "I" [Literal $ Integer 10]) $
    --     LetIn "b" (Constructor "Just" [Variable $ BorrowedVariable "a"]) $
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
    --     -- LetIn "c" (Thunk $ Atom (Variable $ BorrowedVariable "a")) $
    --     LetIn "c" (Thunk $ FunctionApplication (ReferencedVariable "f") Unknown [Variable $ BorrowedVariable "b"]) $
    --     -- LetIn "c" (Thunk $ FunctionApplication (BorrowedVariable "f") Unknown [Variable $ BorrowedVariable "b"]) $
    --     -- LetIn "d" (Thunk $ Atom (Variable $ BorrowedVariable "a")) $
    --     Atom (Variable $ BorrowedVariable "a")
    -- )

    -- Ne sme delovati, ker je main toplevel (staticen)
    -- Binding "main" (Thunk $ Atom $ Variable $ BorrowedVariable "x")

    -- Binding "main" (Thunk $
    --     LetIn "a" (Thunk $ LetIn "b" (Constructor "True" []) (Atom (Variable $ BorrowedVariable "b"))) $
    --     Atom (Variable $ BorrowedVariable "a")
    -- )
    
    -- Binding "main" (Thunk $
    --     LetIn "a" (Constructor "I" [Literal $ Integer 10]) $
    --     LetIn "result" (Thunk $
    --     CaseOf (Atom $ Variable $ BorrowedVariable "a") [
    --         AlgebraicAlternative "True" [] (Atom (Variable $ BorrowedVariable "a")),
    --         DefaultAlternative (BorrowedVariable "x") (Atom (Variable $ BorrowedVariable "x"))
    --     ]) $
    --     Atom (Variable $ BorrowedVariable "result")
    -- )
    
    Binding "main" (Thunk $
        LetIn "f" (Function [BorrowedVariable "x", BorrowedVariable "y"] (Atom $ Literal $ Integer 13)) $
        LetIn "a" (Constructor "Integer" [Literal $ Integer 2]) $
        LetIn "b" (Constructor "Integer" [Literal $ Integer 3]) $
        FunctionApplication (BorrowedVariable "f") Unknown [
            -- Ne deluje, ce sta oba referenci - v tem primeru bi moral tudi
            -- stars prevzeti odgovornost (lastnistvo)
            Variable $ BorrowedVariable "a",
            Variable $ BorrowedVariable "b"
        ]
    )
    ]

main :: IO ()
main = do
    -- Print the program
    print $ map pretty program

    -- Perform name resolution - find undefined variables.
    case nameResolutionProgram program of
        Left (NameResolutionException exception) -> putStrLn exception
        Right _ -> print $ ownership program