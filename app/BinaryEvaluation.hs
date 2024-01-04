import System.Environment

import Types
import Debug.Trace

import qualified Data.ByteString.UTF8 as UTF8
import Data.ByteString (unpack)

import Data.Word (Word8)
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace


-- ; Opcode Definitions
-- LOAD_CONST      0x01
-- LOAD_VAR        0x02
-- STORE_VAR       0x03
-- BINARY_OP       0x04
-- UNARY_OP        0x05
-- COMPARE_OP      0x06
-- JUMP_IF_TRUE    0x07
-- JUMP_IF_FALSE   0x08
-- JUMP            0x09
-- POP             0x0A
-- DUP             0x0B
-- CALL            0x0C
-- RETURN          0x0D
-- BUILD_LIST      0x0E
-- INDEX           0x0F
-- ATTRIBUTE       0x10
-- CREATE_OBJECT   0x11

-- (LoadConst x)
-- (LoadVar x)
-- (StoreVar x)
-- (BinaryOp x)
-- (UnaryOp x)
-- (CompareOp x)
-- (JumpIfTrue x)
-- (JumpIfFalse x)
-- (Jump x)
-- Pop
-- Dup
-- (Call x)
-- Return
-- (BuildList x)
-- Index
-- (Attribute x)
-- (CreateObject x)

data MyElement = MyInt Int | MyString String | MyChar Char deriving (Show)

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

--             ope      stack   new_stack
binaryOpCall :: Word8 -> [Int] -> [Int]
binaryOpCall 43 (x:y:xs) = (x + y) : xs
binaryOpCall 45 (x:y:xs) = (x - y) : xs
-- binaryOpCall "*" (x:y:xs) = (x * y) : xs
-- binaryOpCall "/" (x:y:xs) = (x `div` y) : xs
binaryOpCall _ _ = []

compareOpCall :: Word8 -> [Int] -> [Int]
compareOpCall 60 (x:y:xs) = (if x < y then 1 else 0) : xs
compareOpCall 62 (x:y:xs) = (if x > y then 1 else 0) : xs
compareOpCall 61 (x:y:xs) = (if x == y then 1 else 0) : xs
compareOpCall _ _ = []

-- checkLastInStack :: [Int] -> [Int]
-- checkLastInStack [] = []
-- checkLastInStack (x:xs) = if x == 0 then xs else x : xs


--           opcode   values    stack   (new_stack, PC)
evalValue :: Word8 -> [Word8] -> [Int] -> ([Int], Int)
evalValue 0x01 values stack = trace ("LOAD_CONST "    ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), 1)
evalValue 0x02 values stack = trace ("LOAD_VAR "      ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), 1)
evalValue 0x03 values stack = trace ("STORE_VAR "     ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), 0)
evalValue 0x04 values stack = trace ("BINARY_OP "     ++ show (word8ToInt (values !! 0))) (binaryOpCall (values !! 0) stack, 1)
evalValue 0x05 values stack = trace ("UNARY_OP "      ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), 0)
evalValue 0x06 values stack = trace ("COMPARE_OP "    ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), 0)
evalValue 0x07 values stack = trace ("JUMP_IF_TRUE "  ++ show (word8ToInt (values !! 0))) (if (stack !! 0) /= 0 then (stack, word8ToInt (values !! 0)) else (stack, 1))
evalValue 0x08 values stack = trace ("JUMP_IF_FALSE " ++ show (word8ToInt (values !! 0))) (if (stack !! 0) == 0 then (stack, word8ToInt (values !! 0)) else (stack, 1))
evalValue 0x09 values stack = trace ("JUMP "          ++ show (word8ToInt (values !! 0))) (stack, word8ToInt (values !! 0))
evalValue 0x0A values stack = trace  "POP "                                               ((word8ToInt (values !! 0) : stack), 0)
evalValue 0x0B values stack = trace  "DUP "                                               ((word8ToInt (values !! 0) : stack), 0)
evalValue 0x0C values stack = trace ("CALL "          ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), 0)
evalValue 0x0D values stack = trace  "RETURN "                                            ((word8ToInt (values !! 0) : stack), 0)
evalValue 0x0E values stack = trace ("BUILD_LIST "    ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), 0)
evalValue 0x0F values stack = trace  "INDEX "                                             ((word8ToInt (values !! 0) : stack), 0)
evalValue 0x10 values stack = trace ("ATTRIBUTE "     ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), 0)
evalValue 0x11 values stack = trace ("CREATE_OBJECT " ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), 0)
evalValue _ _ _ = trace "Unknown opcode" ([], 0)

--              bytecodes  stack    (new_bytecodes, new_stack)
evalEachValue :: [Word8] -> [Int] -> ([Word8], [Int])
evalEachValue [] stack = trace ("-- End of bytecodes Stack: " ++ show stack) ([], stack)
evalEachValue (x:xs) stack = do
    let (new_stack, pc) = evalValue x xs stack
    evalEachValue (drop pc xs) new_stack

stringToWord8 :: String -> [Word8]
stringToWord8 str = unpack (UTF8.fromString str)

-- open file given in argument, and print it
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let (bytecodes, stack) = evalEachValue (stringToWord8 contents) []
            putStrLn ("Stack: " ++ show stack)

        _ -> do
            putStrLn "No file given as an argument"
