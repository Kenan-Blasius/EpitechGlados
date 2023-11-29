module Main (main) where

import System.Exit (exitSuccess)
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


-- ( define x 5)
-- x
-- ( if ( > x 4) 1 0)
-- ( define y (+ 5 x ) )

-- read convert String to Int

defineCommand :: Stack -> [String] -> Stack
defineCommand s ( x : y : _ ) = addToStack s (Define x (read y))
defineCommand s _ = s

existInStack :: Stack -> String -> Bool
existInStack [] _ = False
existInStack (Define x _ : rest) str
  | x == str   = True
  | otherwise  = existInStack rest str

-- TODO return 0 if not found, change it
readFromStack :: Stack -> String -> Int
readFromStack [] _ = 0
readFromStack (Define x y : rest) str
  | x == str   = y
  | otherwise  = readFromStack rest str



ifCommand :: Stack -> [String] -> IO ()
ifCommand s ( "1" : valueIfTrue : valueIfFalse : _) = print (readFromStack s valueIfTrue)
ifCommand s ( "0" : valueIfTrue : valueIfFalse : _) = print (readFromStack s valueIfFalse)
ifCommand s _ = putStrLn "Invalid if command"

shellLoop :: Stack -> IO ()
shellLoop s = do
    -- putStrLn $ "Stack with Definition 1: " ++ show s
    putStr "Haskell-Shell> "
    input <- getLine

    case words input of
        ["exit"] -> do
            putStrLn "Goodbye!"
            exitSuccess
        ("define" : args) -> do
            let newStack = defineCommand s args
            shellLoop newStack
        ("if" : args) -> do
            ifCommand s args
            shellLoop s
        [str] -> do
            if existInStack s str
                then print (readFromStack s str)
                else putStrLn $ "String '" ++ str ++ "' does not exist in the stack."
            shellLoop s
        _ -> do
            putStrLn "Invalid command"
            shellLoop s

main :: IO ()
main = do
    putStrLn "Welcome to the Haskell Shell!"
    putStrLn "Type 'exit' to exit."

    -- Start the shell loop
    shellLoop []

    putStrLn "Haskell Project Example"


    -- -- Example 1: Create a stack and add a definition
    -- let initialStack = []
    -- let definition1 = Define "x" 5
    -- let stackWithDefinition1 = addToStack initialStack definition1
    -- putStrLn $ "Stack with Definition 1: " ++ show stackWithDefinition1

    -- -- Example 2: Create a symbolic expression and add another expression to it
    -- let cond_left = Symbol ">"
    -- let cond_midd = Symbol "x"
    -- let cond_right = IntValue 4
    -- let cond = List [cond_left, cond_midd, cond_right]

    -- let operator = Symbol "if"
    -- let expr3 = IntValue 1
    -- let expr4 = IntValue 0
    -- let branch = List [operator, cond, expr3, expr4]

    -- -- let updatedListExpr = addToSExpr listExpr expr3
    -- putStrLn $ "Updated List Expression: " ++ show branch
