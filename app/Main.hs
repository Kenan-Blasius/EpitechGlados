module Main (main) where

import System.Environment
import Types
import AstToBytecode
import BytecodeToBinary
import Parser
import ParserToken (getAbsolutePath)

-- INFO: Main function
main :: IO ()
main = do
    args <- getArgs
    argManager args 0
    where
        argManager :: [String] -> Int -> IO ()
        argManager args n = case args of
            (filename : "-o" : name : remainingArgs) -> do
                -- putStrLn $ "Running file: " ++ filename
                contents <- readFile filename
                file <- return $ File (lines contents)
                ast <- parser file filename
                putStrLn $ show ast
                let (_, bytecode, _) = astToBytecode' ast 0
                -- absoluteFilename <- getAbsolutePath filename
                bytecodeToBinary bytecode name

                argManager remainingArgs (n + 1)

            ("-o" : _ : remainingArgs) -> do
                argManager remainingArgs (n)

            (filename : remainingArgs) -> do
                -- putStrLn $ "Running file: " ++ filename
                contents <- readFile filename
                file <- return $ File (lines contents)
                ast <- parser file filename
                putStrLn $ show ast
                let (_, bytecode, _) = astToBytecode' ast 0
                absoluteFilename <- getAbsolutePath filename
                bytecodeToBinary bytecode absoluteFilename

                argManager remainingArgs (n + 1)

            [] -> case n of
                0 -> do
                    putStrLn "No file given as an argument"
                _ -> do
                    putStrLn "Compilation finished!"

