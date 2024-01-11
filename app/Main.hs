module Main (main) where

import System.Environment
import Types
import AstToBytecode
import BytecodeToBinary
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
            let (_, bytecode, _) = astToBytecode' ast 0
            bytecodeToBinary bytecode

        _ -> do
            putStrLn "No file given as an argument"
