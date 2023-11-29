module Main (main) where

import Lib

data SExpr = IntValue Int
           | Symbol String
           | List [SExpr]
           deriving Show

data Define = Define String Int deriving Show

type Stack = [Define]

addToStack :: Stack -> Define -> Stack
addToStack stack define = define : stack

addToSExpr :: SExpr -> SExpr -> SExpr
addToSExpr (List l) sexpr = List (sexpr : l)


main :: IO ()
main = do
    putStrLn "Haskell Project Example"

    -- Example 1: Create a stack and add a definition
    let initialStack = []
    let definition1 = Define "x" 42
    let stackWithDefinition1 = addToStack initialStack definition1
    putStrLn $ "Stack with Definition 1: " ++ show stackWithDefinition1

    -- Example 2: Create a symbolic expression and add another expression to it
    let expr1 = Symbol "a"
    let expr2 = IntValue 10
    let expr3 = Symbol "b"
    let listExpr = List [expr1, expr2]
    let updatedListExpr = addToSExpr listExpr expr3
    putStrLn $ "Updated List Expression: " ++ show updatedListExpr
