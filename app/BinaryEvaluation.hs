import System.Environment

import Debug.Trace

import qualified Data.ByteString.UTF8 as UTF8
import Data.ByteString (unpack)
import Data.Word (Word8)

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
compareOpCall 60 (x:y:xs) = trace ("x = " ++ show y ++ " < y = " ++ show x) ((if y < x then 1 else 0) : xs)
compareOpCall 62 (x:y:xs) = trace ("x = " ++ show y ++ " > y = " ++ show x) ((if y > x then 1 else 0) : xs)
compareOpCall 61 (x:y:xs) = trace ("x = " ++ show x ++ " == y = " ++ show y) ((if x == y then 1 else 0) : xs)
compareOpCall _ stack = trace ("stack = " ++ show stack) stack

-- checkLastInStack :: [Int] -> [Int]
-- checkLastInStack [] = []
-- checkLastInStack (x:xs) = if x == 0 then xs else x : xs


--           opcode   values    stack     PC    (new_stack, PC)
evalValue :: Word8 -> [Word8] -> [Int] -> Int -> ([Int], Int)
-- * OK
evalValue 0x01 values stack pc = trace ("LOAD_CONST "    ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), pc + 2)
-- * OK
evalValue 0x02 values stack pc = trace ("LOAD_VAR "      ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), pc + 2)
-- TODO
evalValue 0x03 values stack pc = trace ("STORE_VAR "     ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), pc + 2)
-- ? IN PROGRESS
evalValue 0x04 values stack pc = trace ("BINARY_OP "     ++ show (word8ToInt (values !! 0))) (binaryOpCall (values !! 0) stack, pc + 2)
-- TODO
evalValue 0x05 values stack pc = trace ("UNARY_OP "      ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), pc + 2)
-- * OK
evalValue 0x06 values stack pc = trace ("COMPARE_OP "    ++ show (word8ToInt (values !! 0))) (compareOpCall (values !! 0) stack, pc + 2)
-- * OK
evalValue 0x07 values stack pc = trace ("JUMP_IF_TRUE "  ++ show (word8ToInt (values !! 0))) (if (stack !! 0) /= 0 then (stack, word8ToInt (values !! 0)) else (stack, pc + 2))
-- * OK
evalValue 0x08 values stack pc = trace ("JUMP_IF_FALSE " ++ show (word8ToInt (values !! 0))) (if (stack !! 0) == 0 then (stack, word8ToInt (values !! 0)) else (stack, pc + 2))
-- * OK
evalValue 0x09 values stack _ = trace ("JUMP "          ++ show (word8ToInt (values !! 0))) (stack, word8ToInt (values !! 0))
-- TODO
evalValue 0x0A values stack pc = trace  "POP "                                               ((word8ToInt (values !! 0) : stack), pc + 1)
-- TODO
evalValue 0x0B values stack pc = trace  "DUP "                                               ((word8ToInt (values !! 0) : stack), pc + 1)
-- TODO
evalValue 0x0C values stack pc = trace ("CALL "          ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), pc + 2)
-- TODO
evalValue 0x0D _ stack _ = trace  "RETURN "                                            (stack, -1)
-- TODO
evalValue 0x0E values stack pc = trace ("BUILD_LIST "    ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), pc + 2)
-- TODO
evalValue 0x0F values stack pc = trace  "INDEX "                                             ((word8ToInt (values !! 0) : stack), pc + 1)
-- TODO
evalValue 0x10 values stack pc = trace ("ATTRIBUTE "     ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), pc + 2)
-- TODO
evalValue 0x11 values stack pc = trace ("CREATE_OBJECT " ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), pc + 2)
evalValue _ _ _ _ = trace "Unknown opcode" ([], 0)

-- ? we have two bytecodes lists because if we move forward in the list, we can't go back
--              bytecodes  bytecodes  stack      PC   (new_bytecodes, new_stack)
evalEachValue :: [Word8] -> [Word8] -> [Int] -> Int -> ([Word8], [Int])
evalEachValue _ [] stack _ = trace ("-- End of bytecodes Stack: " ++ show stack) ([], stack)
evalEachValue bytecodes (x:xs) stack pc = do
    let (new_stack, new_pc) = evalValue x xs stack pc
    if new_pc == -1 then (bytecodes, new_stack)
    else if new_pc < pc then error "Invalid PC value"
    else
        trace ("pc = " ++ show pc ++ " | " ++ show x ++ " | " ++ show new_stack ++ " | " ++ show (drop pc bytecodes)) $ evalEachValue bytecodes (drop pc bytecodes) new_stack new_pc


stringToWord8 :: String -> [Word8]
stringToWord8 str = unpack (UTF8.fromString str)

-- open file given in argument, and print it
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            let bytecode = stringToWord8 contents
            let (_, stack) = evalEachValue bytecode bytecode [] 0
            putStrLn ("Value returned = " ++ show (stack !! 0))

        _ -> do
            putStrLn "No file given as an argument"
