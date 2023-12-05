module Main (main) where

import System.Environment
import Types
import Eval
import Parser

-- INFO: Main function
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            putStrLn $ "Running file: " ++ filename
            contents <- readFile filename
            file <- return $ File (lines contents)
            ast <- parser file
            putStrLn $ show $ evalAST ast
        _ -> do
            putStrLn "No file given as an argument"
