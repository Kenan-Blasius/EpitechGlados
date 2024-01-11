module Main (main) where

import BinaryEvaluation

import System.Environment
import System.Exit

import qualified Data.ByteString as BS

-- open file given in argument, and print it
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- BS.readFile filename  -- Read binary file
            let bytecode = byteStringToWord8List contents  -- Convert to list of Word8
            if checkMagicNumber (take 4 bytecode) == False then do
                putStrLn "Magic number is incorrect"
                exitWith (ExitFailure 84)
            else do
                let stack = evalEachValue bytecode (drop headerSize bytecode) [] headerSize [[]]
                if length stack < 1 then do
                    putStrLn ("Stack is empty")
                    exitWith (ExitFailure 84)
                else do
                    putStrLn ("Result: " ++ show (getLastIntFromStack stack))
                    exitWith (ExitFailure (getLastIntFromStack stack))

        _ -> putStrLn "No file given as an argument"

-- todo exit with the last int in the stack
