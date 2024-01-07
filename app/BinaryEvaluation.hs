import System.Environment

import Debug.Trace

import qualified Data.ByteString.UTF8 as UTF8
import Data.ByteString (unpack)
import Data.Word (Word8)
import Data.Bits
import Data.List (genericTake)

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

-- (LoadConst x) -- 5
-- (LoadVar x) -- 2
-- (StoreVar x) -- 2
-- (BinaryOp x) -- 2
-- (UnaryOp x) -- 2
-- (CompareOp x) -- 2
-- (JumpIfTrue x) -- 5
-- (JumpIfFalse x) -- 5
-- (Jump x) -- 5
-- Pop -- 1
-- Dup -- 1
-- (Call x) -- 2
-- Return -- 1


data MyElement = MyInt Int | MyString String | MyChar Char deriving (Show)

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

--             ope      stack   new_stack
binaryOpCall :: Word8 -> [Int] -> [Int]
binaryOpCall 43 (x:y:xs) = (x + y) : xs
binaryOpCall 45 (x:y:xs) = (x - y) : xs
binaryOpCall 42 (x:y:xs) = (x * y) : xs
binaryOpCall 47 (x:y:xs) = (x `div` y) : xs
binaryOpCall 37 (x:y:xs) = (x `mod` y) : xs
-- maybe & or | ?
binaryOpCall _ _ = []

compareOpCall :: Word8 -> [Int] -> [Int]
compareOpCall 60 (x:y:xs) = trace ("x = " ++ show y ++ " < y = " ++ show x) ((if y < x then 1 else 0) : xs)
compareOpCall 62 (x:y:xs) = trace ("x = " ++ show y ++ " > y = " ++ show x) ((if y > x then 1 else 0) : xs)
compareOpCall 61 (x:y:xs) = trace ("x = " ++ show x ++ " == y = " ++ show y) ((if x == y then 1 else 0) : xs)
-- binary side
-- >= <= != !
compareOpCall _ stack = trace ("stack = " ++ show stack) stack


bytesToInt :: [Word8] -> Int
bytesToInt bytes =
    fromIntegral (byte 0) +
    (fromIntegral (byte 1) `shiftL` 8) +
    (fromIntegral (byte 2) `shiftL` 16) +
    (fromIntegral (byte 3) `shiftL` 24)
  where
    byte n = genericTake 4 bytes !! n

--           opcode   values    stack     PC    (new_stack, PC)
evalValue :: Word8 -> [Word8] -> [Int] -> Int -> ([Int], Int)
-- * OK
evalValue 0x01 values stack pc = trace ("LOAD_CONST "    ++ show (bytesToInt values)) (((bytesToInt values) : stack), pc + 5)
-- TODO
evalValue 0x02 values stack pc = trace ("LOAD_VAR "      ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), pc + 2)
-- TODO
evalValue 0x03 values stack pc = trace ("STORE_VAR "     ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), pc + 2)
-- TODO
evalValue 0x04 values stack pc = trace ("BINARY_OP "     ++ show (word8ToInt (values !! 0))) (binaryOpCall (values !! 0) stack, pc + 2)
-- TODO
evalValue 0x05 values stack pc = trace ("UNARY_OP "      ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), pc + 2)
-- * OK
evalValue 0x06 values stack pc = trace ("COMPARE_OP "    ++ show (word8ToInt (values !! 0))) (compareOpCall (values !! 0) stack, pc + 2)
-- * OK
evalValue 0x07 values stack pc = trace ("JUMP_IF_TRUE "  ++ show (bytesToInt values)) (if (stack !! 0) /= 0 then (stack, (bytesToInt values)) else (stack, pc + 5))
-- * OK
evalValue 0x08 values stack pc = trace ("JUMP_IF_FALSE " ++ show (bytesToInt values)) (if (stack !! 0) == 0 then (stack, (bytesToInt values)) else (stack, pc + 5))
-- * OK
evalValue 0x09 values stack _ = trace ("JUMP "          ++ show (bytesToInt values)) (stack, bytesToInt values)
-- TODO, Pop the top value from the stack.
evalValue 0x0A values stack pc = trace  "POP "                                               ((word8ToInt (values !! 0) : stack), pc + 1)
-- TODO, Duplicate the top value on the stack.
evalValue 0x0B values stack pc = trace  "DUP "                                               ((word8ToInt (values !! 0) : stack), pc + 1)
-- TODO, if x == 1, print, if x == 60, exit
evalValue 0x0C values stack pc = trace ("CALL "          ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), pc + 2)
-- TODO, return the value at the top of the stack to the caller.
evalValue 0x0D _ stack _ = trace  "RETURN "                                            (stack, -1)
evalValue a b c d = trace ("Unknown opcode: " ++ show a ++ " | values: " ++ show b ++ " | stack: " ++ show c ++ " | pc: " ++ show d) ([], -1)

-- ? we have two bytecodes lists because if we move forward in the list, we can't go back
--              bytecodes  bytecodes  stack      PC   (new_bytecodes, new_stack)
evalEachValue :: [Word8] -> [Word8] -> [Int] -> Int -> ([Word8], [Int])
evalEachValue _ [] stack _ = trace ("-- End of bytecodes Stack: " ++ show stack) ([], stack)
evalEachValue bytecodes (x:xs) stack pc = do
    let (new_stack, new_pc) = trace ("evalValue executed with pc = " ++ show pc ++ " | opcode = " ++ show x) $ evalValue x xs stack pc
    if new_pc == -1 then (bytecodes, new_stack)
    else
        trace ("pc = " ++ show pc ++ " | opcode = " ++ show x ++ " | stack = " ++ show stack ++ " | new_stack = " ++ show new_stack ++ " | new_pc = " ++ show new_pc)
        $ evalEachValue bytecodes (drop new_pc bytecodes) new_stack new_pc


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


-- not forgot to remove value from stack when we use it
-- handle several bytes as a value