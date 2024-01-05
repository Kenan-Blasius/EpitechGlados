module Main (main) where

import System.Environment
import Types
-- import Eval
import Parser

-- INFO: Main function
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            -- putStrLn $ "Running file: " ++ filename
            contents <- readFile filename
            file <- return $ File (lines contents)
            ast <- parser file filename
            putStrLn $ show ast
            -- let env = []
            -- let (_, evaluatedAST) = evalAST env ast
            -- print (evaluatedAST)
        _ -> do
            putStrLn "No file given as an argument"
