import System.Environment

import Debug.Trace

import qualified Data.ByteString as BS
import Data.ByteString (unpack)
import Data.Word (Word8)
import Data.Bits
import Data.List (genericTake)
import Data.Char
import Data.Int (Int32)

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


word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

intToChar :: Int -> Char
intToChar = chr

--             ope      stack   new_stack
binaryOpCall :: Word8 -> [Int] -> [Int]
binaryOpCall 43 (x:y:xs) = (x + y) : xs
binaryOpCall 45 (x:y:xs) = (y - x) : xs
binaryOpCall 42 (x:y:xs) = (x * y) : xs
binaryOpCall 47 (x:y:xs) = (y `div` x) : xs
binaryOpCall 37 (x:y:xs) = (y `mod` x) : xs
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
    let val :: Int32
        val = fromIntegral (byte 0) .|.
              (fromIntegral (byte 1) `shiftL` 8) .|.
              (fromIntegral (byte 2) `shiftL` 16) .|.
              (fromIntegral (byte 3) `shiftL` 24)
    in if testBit val 31  -- Teste si le bit de signe (32ème bit) est activé
       then fromIntegral (val - (1 `shiftL` 32 :: Int32))  -- Ajuste pour les nombres négatifs
       else fromIntegral val
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


byteStringToWord8List :: BS.ByteString -> [Word8]
byteStringToWord8List = unpack

-- open file given in argument, and print it
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- BS.readFile filename  -- Lire le fichier binaire
            let bytecode = byteStringToWord8List contents  -- Convertir en liste de Word8
            let (_, stack, _) = evalEachValue bytecode bytecode [] 0 []
            putStrLn ("Value returned = " ++ show (stack !! 0))

        _ -> putStrLn "No file given as an argument"


-- not forgot to remove value from stack when we use it
-- handle several bytes as a value

-- TODO PRECISEION OF TYPE (int, string, bool) OF VARIABLES IN BYTECODE !!!
-- DECLARE_INT a
-- DECLARE_STRING b
-- DECLARE_BOOL c