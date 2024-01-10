import System.Environment
import System.Exit

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
-- JUMP_NEW_SCOPE  0x0A
-- POP             0x0B
-- DUP             0x0C
-- CALL            0x0D
-- RETURN          0x0E

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

headerSize :: Int
headerSize = 32

type VariableName = String
data VariableType = IntType | StringType | BoolType | CharType | FloatType | AddressType deriving (Show)

data VariableElement = MyInt Int | MyString String | MyChar Char | MyBool Bool | MyFloat Float deriving (Show)

type VariableEntry = (VariableName, VariableType, VariableElement)
type VariableTable = [VariableEntry]


type StackEntry = (VariableType, VariableElement)
type StackTable = [StackEntry]


word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

intToChar :: Int -> Char
intToChar = chr

--             ope      stack   new_stack
binaryOpCall :: Word8 -> StackTable -> StackTable
binaryOpCall 43 ((_, MyInt y) : (_, MyInt x) : xs) = (IntType, MyInt (x + y)) : xs
binaryOpCall 45 ((_, MyInt y) : (_, MyInt x) : xs) = (IntType, MyInt (x - y)) : xs
binaryOpCall 42 ((_, MyInt y) : (_, MyInt x) : xs) = (IntType, MyInt (x * y)) : xs
binaryOpCall 47 ((_, MyInt y) : (_, MyInt x) : xs) = (IntType, MyInt (x `div` y)) : xs
binaryOpCall 37 ((_, MyInt y) : (_, MyInt x) : xs) = (IntType, MyInt (x `mod` y)) : xs
binaryOpCall 38 ((_, MyInt y) : (_, MyInt x) : xs) = (IntType, MyInt (x .&. y)) : xs
binaryOpCall 124 ((_, MyInt y) : (_, MyInt x) : xs) = (IntType, MyInt (x .|. y)) : xs
binaryOpCall 94 ((_, MyInt y) : (_, MyInt x) : xs) = (IntType, MyInt (x `xor` y)) : xs
binaryOpCall _ stack = stack  -- Default case, no operation for other Word8 values
-- maybe & or | ?

compareOpCall :: Word8 -> StackTable -> StackTable
compareOpCall 60 ((_, MyInt y) : (_, MyInt x) : xs) =
    trace ("stack : top = " ++ show y ++ " > x = " ++ show x) ((if x < y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 62 ((_, MyInt y) : (_, MyInt x) : xs) =
    trace ("stack : top = " ++ show y ++ " < x = " ++ show x) ((if x > y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 61 ((_, MyInt y) : (_, MyInt x) : xs) =
    trace ("stack : top = " ++ show y ++ " == x = " ++ show x) ((if x == y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 33 ((_, MyInt y) : (_, MyInt x) : xs) =
    trace ("stack : top = " ++ show y ++ " != x = " ++ show x) ((if x /= y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall x stack = trace ("ERROR COMPARE OP : " ++ show x ++ " | stack : " ++ show stack) stack

-- binary side
-- >= <= != !


lenOp :: Word8 -> Int
lenOp 0x01 = 6 -- LOAD_CONST     (int 4, type 1)
lenOp 0x02 = 3 -- LOAD_VAR       (int 1, type 1)
lenOp 0x03 = 3 -- STORE_VAR      (int 1, type 1)
lenOp 0x04 = 2 -- BINARY_OP      (int 1)
lenOp 0x05 = 2 -- UNARY_OP       (int 1)
lenOp 0x06 = 2 -- COMPARE_OP     (int 1)
lenOp 0x07 = 5 -- JUMP_IF_TRUE   (int 4)
lenOp 0x08 = 5 -- JUMP_IF_FALSE  (int 4)
lenOp 0x09 = 5 -- JUMP           (int 4)
lenOp 0x0A = 5 -- JUMP_NEW_SCOPE (int 4)
lenOp 0x0B = 1 -- POP            ()
lenOp 0x0C = 1 -- DUP            ()
lenOp 0x0D = 2 -- CALL           (int 1)
lenOp 0x0E = 1 -- RETURN         ()
lenOp 0x0F = 1 -- LOAD_PC        ()
lenOp _ = 0


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


getLastIntFromStack :: StackTable -> Int
getLastIntFromStack [] = 0
getLastIntFromStack ((IntType, MyInt x):_) = x
getLastIntFromStack ((_, _):xs) = trace "Not an int, go next in stack" $ getLastIntFromStack xs

deleteLastIntFromStack :: StackTable -> StackTable
deleteLastIntFromStack [] = []
deleteLastIntFromStack ((IntType, _):xs) = xs
deleteLastIntFromStack (x:xs) = x : deleteLastIntFromStack xs

getLastAddressFromStack :: StackTable -> Int
getLastAddressFromStack [] = -1
getLastAddressFromStack ((AddressType, MyInt x):_) = x
getLastAddressFromStack ((_, _):xs) = trace "Not an address, go next in stack" $ getLastAddressFromStack xs

deleteUntilAddress :: StackTable -> StackTable
deleteUntilAddress [] = []
deleteUntilAddress ((AddressType, _):xs) = xs
deleteUntilAddress (_:xs) = deleteUntilAddress xs

deleteUntilAddressExceptOne :: StackTable -> Int -> StackTable
deleteUntilAddressExceptOne [] _ = []
deleteUntilAddressExceptOne (x:xs) 0 = x : deleteUntilAddress xs
deleteUntilAddressExceptOne ((AddressType, _):xs) _ = xs
deleteUntilAddressExceptOne (_:xs) n = deleteUntilAddressExceptOne xs (n + 1)


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


-- dataTypeToByte :: DataType -> Word8
-- dataTypeToByte IntType = 0x01
-- dataTypeToByte StringType = 0x02
-- dataTypeToByte BoolType = 0x03
-- dataTypeToByte FloatType = 0x04
-- dataTypeToByte VoidType = 0x05
-- dataTypeToByte CharType = 0x06
-- dataTypeToByte FunType = 0x07

loadConst :: [Word8] -> StackEntry
loadConst (a:b:c:d:t:_) | t == 0x01 = (IntType, MyInt       (bytesToInt                [a, b, c, d]))
-- loadConst (a:b:c:d:t:_) | t == 0x02 = (StringType, MyString (intToChar (bytesToInt [a, b, c, d])))
-- loadConst (a:b:c:d:t:_) | t == 0x03 = (BoolType, MyBool     (bytesToInt                [a, b, c, d]))
loadConst (a:b:c:d:t:_) | t == 0x04 = (FloatType, MyFloat   (fromIntegral  (bytesToInt [a, b, c, d])))
loadConst (a:b:c:d:t:_) | t == 0x05 = (AddressType, MyInt   (bytesToInt                [a, b, c, d]))
loadConst (a:b:c:d:t:_) | t == 0x06 = (CharType, MyChar     (intToChar     (bytesToInt [a, b, c, d])))
loadConst _ = trace "ERROR LOAD CONST" (IntType, MyInt 0)


printValueInStack :: StackEntry -> String
printValueInStack (IntType, MyInt x) = show x
printValueInStack (FloatType, MyFloat x) = show x
printValueInStack (CharType, MyChar x) = show x
printValueInStack (AddressType, MyInt x) = show x
printValueInStack _ = "ERROR PRINT VALUE IN STACK"

-- * ---------------------------------------------- EVAL ----------------------------------------------

-- ? stack is global, JUMP_NEW_SCOPE is useless ?
--           opcode   values    stack     PC    VariableTable    (new_stack, new_pc, new_VariableTable)
evalValue :: Word8 -> [Word8] -> StackTable -> Int -> VariableTable -> (StackTable, Int, VariableTable)
evalValue 0x01 values stack pc table = trace ("LOAD_CONST "    ++ show (bytesToInt values))        (loadConst values : stack, pc + lenOp 0x01, table)
evalValue 0x02 values stack pc table = trace ("LOAD_VAR "      ++ show (word8ToInt (head values))) ((IntType, (MyInt (getIntFromVariable [intToChar (word8ToInt (head values))] table))) : stack, pc + lenOp 0x02, table)
evalValue 0x03 values stack pc table = trace ("STORE_VAR "     ++ show (word8ToInt (head values))) (deleteLastIntFromStack stack, pc + lenOp 0x03, updateVariable [intToChar (word8ToInt (head values))] IntType (MyInt (getLastIntFromStack stack)) table)
-- TODO && || !
evalValue 0x04 values stack pc table = trace ("BINARY_OP "     ++ show (word8ToInt (head values))) (binaryOpCall (head values) stack, pc + lenOp 0x04, table)
-- TODO
evalValue 0x05 values stack pc table = trace ("UNARY_OP "      ++ show (word8ToInt (head values))) (((IntType, (MyInt (word8ToInt (head values)))) : stack), pc + lenOp 0x05, table)
evalValue 0x06 values stack pc table = trace ("COMPARE_OP "    ++ show (word8ToInt (head values))) (compareOpCall (head values) stack, pc + lenOp 0x06, table)
evalValue 0x07 values stack pc table = trace ("JUMP_IF_TRUE "  ++ show (bytesToInt values))        (if (getLastIntFromStack stack) /= 0 then (deleteLastIntFromStack stack, (bytesToInt values), table) else (deleteLastIntFromStack stack, pc + lenOp 0x07, table))
evalValue 0x08 values stack pc table = trace ("JUMP_IF_FALSE " ++ show (bytesToInt values))        (if (getLastIntFromStack stack) == 0 then (deleteLastIntFromStack stack, (bytesToInt values), table) else (deleteLastIntFromStack stack, pc + lenOp 0x08, table))
evalValue 0x09 values stack _ table = trace ("JUMP "          ++ show (bytesToInt values))         (stack, bytesToInt values, table)
-- TODO new table frame + (take last x values from stack)
-- ! CREATE A NEW VARIABLE TABLE
evalValue 0x0A values stack _ table = trace  ("JUMP_NEW_SCOPE " ++ show (bytesToInt values))       (stack, bytesToInt values, table)
evalValue 0x0B _ stack pc table = trace  "POP "                                                    (tail stack, pc + lenOp 0x0B, table)
evalValue 0x0C _ (s:stack) pc table = trace  "DUP "                                                (s : s : stack, pc + lenOp 0x0C, table)
-- * x == 1, print -- x == 60, exit
evalValue 0x0D (1:_) (x:xs) pc table = trace ("CALL 1: " ++ printValueInStack x)                   (xs, pc + lenOp 0x0D, table)
evalValue 0x0D (60:_) _ _ _ = trace "EXIT"                                                         ([], -1, [])
-- ! TAKE THE LAST VARIABLE TABLE
evalValue 0x0E _ stack _ table = trace  "RETURN "                                                  (deleteUntilAddressExceptOne stack 0, getLastAddressFromStack stack, table)
evalValue 0x0F _ stack pc table = trace "LOAD_PC "                                                 (((AddressType, (MyInt (pc + 1 + 5))) : stack), pc + lenOp 0x0F, table) -- ! pc + 5 because LOAD_PC + JUMP_
evalValue a b c d e = trace ("Unknown opcode: " ++ show a ++ " | values: " ++ show b ++ " | stack: " ++ show c ++ " | pc: " ++ show d ++ " | table: " ++ show e) ([], -1, [])

-- ? we have two bytecodes lists because if we move forward in the list, we can't go back
--              bytecodes  bytecodes  stack      PC   VariableTable    -> stack
evalEachValue :: [Word8] -> [Word8] -> StackTable -> Int -> [VariableTable] -> StackTable
evalEachValue _ [] stack _ _ = trace ("-- End of bytecodes Stack: " ++ show stack) stack
evalEachValue bytecodes (x:xs) stack pc tables = do
    let (new_stack, new_pc, new_table) = evalValue x xs stack pc (head tables) -- ? head or tail ?
    let debugInfo = "pc = " ++ show pc ++ " | stack = " ++ show new_stack ++ " | next pc = " ++ show new_pc ++ " | table = " ++ show new_table
    if new_pc == -1 then
        stack
    else
        if x == 0x0A then
            trace debugInfo $ evalEachValue bytecodes (drop new_pc bytecodes) new_stack new_pc ([] : new_table : (tail tables))
        else if x == 0x0E then
            trace debugInfo $ evalEachValue bytecodes (drop new_pc bytecodes) new_stack new_pc (tail tables)
        else
            trace debugInfo $ evalEachValue bytecodes (drop new_pc bytecodes) new_stack new_pc (new_table : (tail tables))


byteStringToWord8List :: BS.ByteString -> [Word8]
byteStringToWord8List = unpack
--[0x7a, 0x69, 0x7a, 0x69]
checkMagicNumber :: [Word8] -> Bool
checkMagicNumber [0x7a, 0x69, 0x7a, 0x69] = True
checkMagicNumber _ = False

-- open file given in argument, and print it
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- BS.readFile filename  -- Lire le fichier binaire
            let bytecode = byteStringToWord8List contents  -- Convertir en liste de Word8
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
                    exitWith (ExitSuccess)

        _ -> putStrLn "No file given as an argument"


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