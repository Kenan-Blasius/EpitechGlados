import System.Environment
import System.Exit

import Debug.Trace

import qualified Data.ByteString.UTF8 as UTF8
import Data.ByteString (unpack)
import Data.Word (Word8)
import Data.Bits
import Data.List (genericTake)
import Data.Char

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

type VariableName = String
data VariableType = IntType | StringType | BoolType deriving (Show)

data VariableElement = MyInt Int | MyString String | MyChar Char deriving (Show)

type VariableEntry = (VariableName, VariableType, VariableElement)
type VariableTable = [VariableEntry]


type StackEntry = (VariableType, VariableElement)
type StackTable = [StackEntry]


word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

intToChar :: Int -> Char
intToChar = chr

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




-- Function to look up a variable in the table
lookupVariable :: VariableName -> VariableTable -> Maybe VariableEntry
lookupVariable _ [] = Nothing
lookupVariable name ((n, t, e):xs)
    | name == n = Just (n, t, e)
    | otherwise = lookupVariable name xs


getIntFromVariable :: VariableName -> VariableTable -> Int
getIntFromVariable name table = case lookupVariable name table of
    Just (_, _, MyInt x) -> x
    _ -> 0

-- Function to update the value of an existing variable in the table
updateVariable :: VariableName -> VariableType -> VariableElement -> VariableTable -> VariableTable
updateVariable name varType element [] = [(name, varType, element)]
updateVariable name varType element ((n, t, e):xs)
    | name == n = (name, varType, element) : xs
    | otherwise = (n, t, e) : updateVariable name varType element xs





bytesToInt :: [Word8] -> Int
bytesToInt bytes =
    fromIntegral (byte 0) +
    (fromIntegral (byte 1) `shiftL` 8) +
    (fromIntegral (byte 2) `shiftL` 16) +
    (fromIntegral (byte 3) `shiftL` 24)
  where
    byte n = genericTake (4 :: Int) bytes !! n

--           opcode   values    stack     PC    VariableTable    (new_stack, new_pc, new_VariableTable)
evalValue :: Word8 -> [Word8] -> [Int] -> Int -> VariableTable -> ([Int], Int, VariableTable)
-- * OK
evalValue 0x01 values stack pc table = trace ("LOAD_CONST "    ++ show (bytesToInt values)) (((bytesToInt values) : stack), pc + 5, table)
-- TODO
evalValue 0x02 values stack pc table = trace ("LOAD_VAR "      ++ show (word8ToInt (values !! 0))) ((getIntFromVariable [intToChar (word8ToInt (values !! 0))] table : stack), pc + 2, table)
-- TODO
evalValue 0x03 values stack pc table = trace ("STORE_VAR "     ++ show (word8ToInt (values !! 0))) (stack, pc + 2, updateVariable [intToChar (word8ToInt (values !! 0))] IntType (MyInt (stack !! 0)) table)
-- TODO
evalValue 0x04 values stack pc table = trace ("BINARY_OP "     ++ show (word8ToInt (values !! 0))) (binaryOpCall (values !! 0) stack, pc + 2, table)
-- TODO
evalValue 0x05 values stack pc table = trace ("UNARY_OP "      ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), pc + 2, table)
-- * OK
evalValue 0x06 values stack pc table = trace ("COMPARE_OP "    ++ show (word8ToInt (values !! 0))) (compareOpCall (values !! 0) stack, pc + 2, table)
-- * OK
evalValue 0x07 values stack pc table = trace ("JUMP_IF_TRUE "  ++ show (bytesToInt values)) (if (stack !! 0) /= 0 then (stack, (bytesToInt values), table) else (stack, pc + 5, table))
-- * OK
evalValue 0x08 values stack pc table = trace ("JUMP_IF_FALSE " ++ show (bytesToInt values)) (if (stack !! 0) == 0 then (stack, (bytesToInt values), table) else (stack, pc + 5, table))
-- * OK
evalValue 0x09 values stack _ table = trace ("JUMP "          ++ show (bytesToInt values)) (stack, bytesToInt values, table)
-- TODO, Pop the top value from the stack.
evalValue 0x0A values stack pc table = trace  "POP "                                               ((word8ToInt (values !! 0) : stack), pc + 1, table)
-- TODO, Duplicate the top value on the stack.
evalValue 0x0B values stack pc table = trace  "DUP "                                               ((word8ToInt (values !! 0) : stack), pc + 1, table)
-- TODO, if x == 1, print, if x == 60, exit
evalValue 0x0C values stack pc table = trace ("CALL "          ++ show (word8ToInt (values !! 0))) ((word8ToInt (values !! 0) : stack), pc + 2, table)
-- TODO, return the value at the top of the stack to the caller.
evalValue 0x0D _ stack _ table = trace  "RETURN "                                            (stack, -1, table)
evalValue a b c d e = trace ("Unknown opcode: " ++ show a ++ " | values: " ++ show b ++ " | stack: " ++ show c ++ " | pc: " ++ show d ++ " | table: " ++ show e) ([], -1, [])

-- ? we have two bytecodes lists because if we move forward in the list, we can't go back
--              bytecodes  bytecodes  stack      PC   VariableTable     (new_bytecodes, new_stack, new_VariableTable)
evalEachValue :: [Word8] -> [Word8] -> [Int] -> Int -> VariableTable -> ([Word8], [Int], VariableTable)
evalEachValue _ [] stack _ _ = trace ("-- End of bytecodes Stack: " ++ show stack) ([], stack, [])
evalEachValue bytecodes (x:xs) stack pc table = do
    let (new_stack, new_pc, new_table) = trace ("evalValue executed with pc = " ++ show pc ++ " | opcode = " ++ show x) $ evalValue x xs stack pc table
    if new_pc == -1 then ([], new_stack, [])
    else
        trace ("pc = " ++ show pc ++ " | opcode = " ++ show x ++ " | stack = " ++ show stack ++ " | new_stack = " ++ show new_stack ++ " | new_pc = " ++ show new_pc ++ " | new_table = " ++ show new_table)
        $ evalEachValue bytecodes (drop new_pc bytecodes) new_stack new_pc new_table


--[0x7a, 0x69, 0x7a, 0x69]
checkMagicNumber :: [Word8] -> Bool
checkMagicNumber [0x7a, 0x69, 0x7a, 0x69] = True
checkMagicNumber _ = False

stringToWord8 :: String -> [Word8]
stringToWord8 str = unpack (UTF8.fromString str)

-- open file given in argument, and print it
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            -- putStrLn "Magic number check"
            let bytecode = stringToWord8 contents
            if checkMagicNumber (take 4 bytecode) == False then do
                putStrLn "Magic number is incorrect"
                exitWith (ExitFailure 84)
            else do
                let (_, stack, _) = evalEachValue bytecode (drop 32 bytecode) [] 32 []
                if length stack < 1 then do
                    putStrLn ("Stack is empty")
                    exitWith (ExitFailure 84)
                else do
                    putStrLn ("Result: " ++ show (stack !! 0))
                    exitWith (ExitSuccess)

        _ -> do
            putStrLn "No file given as an argument"


-- not forgot to remove value from stack when we use it
-- handle several bytes as a value

-- TODO PRECISEION OF TYPE (int, string, bool) OF VARIABLES IN BYTECODE !!!
-- DECLARE_TYPE INT a
-- DECLARE_TYPE STRING b
-- DECLARE_TYPE BOOL c

-- LOAD_CONST 1
-- |
-- 1,2, 1,0,0,0,
--   |  -------
--   type (value)

-- STORE_VAR 1
-- |
-- 3,1, 1,0,0,0,
--   |  -------
--  type (value)


-- 1 -> int
-- 2 -> string
-- 3 -> bool
-- 4 -> char
-- 5 -> float
-- 6 -> function

-- 7 -> list ?

-- * (stack table) -> (local table) -> (global table)


-- todo all local variables are stored in (local table)
-- todo read the bytecode a first time to get all the FunEntryPoint and store them in the (global table)

-- todo type of values in the stack