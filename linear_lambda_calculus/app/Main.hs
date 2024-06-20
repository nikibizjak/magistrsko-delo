{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Main where

import Types
import Parser

splitContext :: String -> Context -> (Context, Context)
splitContext variable context =
    case context of
        [] -> ([], [])
        (name, _) : rest | name == variable -> ([], rest)
        first : rest ->
            let (before, after) = splitContext variable rest in
                (first : before, after)

isMoreRestricted :: Qualifier -> Qualifier -> Bool
isMoreRestricted Linear Unrestricted = True
isMoreRestricted Linear Linear = True
isMoreRestricted Unrestricted Unrestricted = True
isMoreRestricted _ _ = False

checkQualifier :: Qualifier -> Type -> Bool
checkQualifier q (Type q' _) = isMoreRestricted q q'

contextDifference :: Context -> Context -> Context
contextDifference first second =
    contextDifferenceInner (reverse first) (reverse second)

contextDifferenceInner :: Context -> Context -> Context
contextDifferenceInner context1 [] = reverse context1

contextDifferenceInner context1 ((x, Type Unrestricted p) : context2) =
    let context3 = contextDifference context1 context2 in
        let (left, right) = splitContext x context3 in
            reverse left ++ reverse right

contextDifferenceInner context1 ((x, Type Linear p) : context2) =
    let context3 = contextDifference context1 context2 in
        if (x, Type Linear p) `elem` context3
        then [] -- Error
        else reverse context3

typeCheck :: [(String, Type)] -> Term -> Either TypeCheckException (Type, Context)
typeCheck context (Variable name)
    | case lookup name context of
        Just (Type Unrestricted _) -> True
        _ -> False
    =
        let
            Just typ = lookup name context
        in
            Right (typ, context)

typeCheck context (Variable name)
    | case lookup name context of
        Just (Type Linear _) -> True
        _ -> False
    =
        let
            Just typ = lookup name context
            (context1, context2) = splitContext name context
        in
            Right (typ, context1 ++ context2)

typeCheck context (Boolean qualifier _) =
    Right (Type qualifier TBoolean, context)

typeCheck context (Conditional condition thenBranch elseBranch) =
    case typeCheck context condition of
        Left exception -> Left exception
        Right (Type qualifier TBoolean, conditionContext) ->
            case typeCheck conditionContext thenBranch of
                Left exception -> Left exception
                Right (thenType, thenContext) ->
                    case typeCheck conditionContext elseBranch of
                        Left exception -> Left exception
                        Right (elseType, elseContext) ->
                            if thenType /= elseType
                            then throw "Then and else branches don't return the same type."
                            else
                                if thenContext /= elseContext
                                then throw "Then and else contexts don't match."
                                else Right (thenType, thenContext)

        Right (typ, _) ->
            throw $ "Condition type is '" ++ show typ ++ "'."

typeCheck context (Pair qualifier left right) =
    case typeCheck context left of
        Left exception -> Left exception
        Right (leftType, leftContext) ->
            case typeCheck context right of
                Left exception -> Left exception
                Right (rightType, rightContext) ->
                    if not (checkQualifier qualifier leftType)
                    then throw "Left qualifier invalid."
                    else
                        if not (checkQualifier qualifier rightType)
                        then throw "Right qualifier invalid."
                        else Right (Type qualifier $ TPair leftType rightType, rightContext)

typeCheck context (Split pair x y body) =
    case typeCheck context pair of
        Left exception -> Left exception
        Right (Type qualifier (TPair leftType rightType), pairContext) ->
            let
                context' = (x, leftType) : (y, rightType) : pairContext
            in
                case typeCheck context' body of
                    Left exception -> Left exception
                    Right (bodyType, bodyContext) ->
                        Right (bodyType, contextDifference bodyContext [ (x, leftType), (y, rightType) ])
        Right (pairType, _) ->
            throw $ "Can't split term that is not a pair. Actual type is '" ++ show pairType ++ "'."

typeCheck context (Abstraction qualifier parameter typ body) =
    let context' = (parameter, typ) : context in
        case typeCheck context' body of
            Left exception -> Left exception
            Right (bodyType, bodyContext) ->
                case qualifier of
                    Unrestricted ->
                        if context /= contextDifference bodyContext [(parameter, typ)]
                        then throw "Error"
                        else
                            Right (Type qualifier (TFunction typ bodyType), contextDifference bodyContext [(parameter, typ)])
                    Linear -> todo

typeCheck context (Application left right) =
    todo

typeCheck _ _ =
    todo

throw :: String -> Either TypeCheckException b
throw a = Left $ TypeCheckException a

todo :: Either TypeCheckException b
todo = throw "TODO"

program :: String
program = "(un \\ x : un Bool . lin true) false"

main :: IO ()
main =
    case parse program of
        Left (ParserException exception) -> putStrLn exception
        Right term -> do
            print term
            case typeCheck [] term of
                Left (TypeCheckException exception) ->
                    putStrLn exception
                Right result ->
                    print result