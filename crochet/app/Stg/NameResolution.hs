module Stg.NameResolution where

import Stg.Stg

newtype NameResolutionException = NameResolutionException String

class NameResolution a where
  nameResolution :: [String] -> a -> Either NameResolutionException ()

nameResolutionProgram :: Program -> Either NameResolutionException ()
nameResolutionProgram program =
    let environment = map (\(Binding name object) -> name) program in
        nameResolutionMultiple environment program

findVariable :: [String] -> String -> Either NameResolutionException ()
findVariable environment name =
    if name `elem` environment
        then Right ()
        else Left (NameResolutionException $ "Undefined variable '" ++ name ++ "'.")

instance NameResolution Literal where
    nameResolution _ _ = Right ()

instance NameResolution Atom where
    nameResolution environment (Literal literal) =
        nameResolution environment literal
    nameResolution environment (Variable name) =
        findVariable environment name

instance NameResolution Expression where
    nameResolution environment (Atom atom) = nameResolution environment atom
    nameResolution environment (FunctionApplication function _ arguments) =
        case findVariable environment function of
            Left exception -> Left exception
            Right _ -> nameResolutionMultiple environment arguments
    nameResolution environment (PrimitiveOperation _ arguments) =
        nameResolutionMultiple environment arguments
    nameResolution environment (LetIn name object body) =
        let environment' = name : environment in
        case nameResolution environment' object of
            Left exception -> Left exception
            Right _ -> nameResolution environment' body
    nameResolution environment (CaseOf scrutinee alternatives) =
        case nameResolution environment scrutinee of
            Left exception -> Left exception
            Right _ -> nameResolutionMultiple environment alternatives

instance NameResolution Alternative where
    nameResolution environment (AlgebraicAlternative _ parameters body) =
        nameResolution (names parameters ++ environment) body
    nameResolution environment (DefaultAlternative variable body) =
        nameResolution (name variable : environment) body

instance NameResolution Object where
  nameResolution environment (Function parameters body) =
    nameResolution (names parameters ++ environment) body
  nameResolution environment (PartialApplication function arguments) =
    case findVariable environment function of
        Left exception -> Left exception
        Right _ -> nameResolutionMultiple environment arguments
  nameResolution environment (Constructor _ arguments) = nameResolutionMultiple environment arguments
  nameResolution environment (Thunk expression) = nameResolution environment expression
  nameResolution environment BlackHole = Right ()

instance NameResolution Binding where
  nameResolution environment (Binding name value) = nameResolution (name : environment) value

name :: Variable -> String
name variable = variable

names :: [Variable] -> [String]
names = map name

nameResolutionMultiple :: (NameResolution a) => [String] -> [a] -> Either NameResolutionException ()
nameResolutionMultiple environment items =
    case traverse (nameResolution environment) items of
        Right _ -> Right ()
        Left exception -> Left exception