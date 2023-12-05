module Main (main) where

import System.Environment
import Types
import Eval
import Parser

-- INFO: Main function
main :: IO ()
main = do
--     -- Hardcoded test
--     let ast = IfAST (AST [SymbolAST ">=", SymbolAST "103", IntAST 103]) (AST [SymbolAST "True"]) (AST [IntAST 0])
--     putStrLn $ "AST: " ++ printAST ast
--     putStrLn $ "Result: " ++ printAST (evalAST ast)


-- Kenan's code
-- AST: IfAST (SymbolAST "=") (IntAST 0) (IntAST 2)
-- evalAST IfAST (SymbolAST "=") (IntAST 0) (IntAST 2)
-- evalAST SymbolAST "="
-- Symbol: =
-- Result: Nothing
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
