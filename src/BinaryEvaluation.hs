module BinaryEvaluation (evalEachValue, byteStringToWord8List, checkMagicNumber, headerSize, getLastIntFromStack,
    VariableId, VariableType (..), VariableElement (..), VariableEntry, VariableTable, StackEntry, StackTable,
    intToFloat, word8ToInt, intToChar, lenOp, binaryOpCall, unaryOpCall, compareOpCall, getFromVariable, updateVariable,
    new_VariableTable, deleteLastValueFromStack, getLastAddressFromStack, deleteUntilAddress, deleteUntilAddressExceptOne,
    toNewVariable, bytesToInt, toStringFromHere, word8withAdresstoString, loadConst, printValueInStack, getNthValue,
    ) where

import Debug.Trace

import qualified Data.ByteString as BS
import Data.ByteString (unpack)
import Data.Word (Word8)
import Data.Bits
import Data.List (genericTake)
import Data.Char
import Data.Int (Int32)
import Unsafe.Coerce

type VariableId = Int
data VariableType = IntType | StringType | BoolType | CharType | FloatType | AddressType deriving (Show, Eq)
data VariableElement = MyInt Int | MyString String | MyChar Char | MyBool Bool | MyFloat Float deriving (Show, Eq)


type VariableEntry = (VariableId, VariableType, VariableElement)
type VariableTable = [VariableEntry]


type StackEntry = (VariableType, VariableElement)
type StackTable = [StackEntry]

headerSize :: Int
headerSize = 32

intToFloat :: Int -> Float
intToFloat = unsafeCoerce

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral

intToChar :: Int -> Char
intToChar = chr

lenOp :: Word8 -> Int
lenOp 0x01 = 6 -- LOAD_CONST     (int 4, type 1)
lenOp 0x02 = 6 -- LOAD_VAR       (int 4, type 1)
lenOp 0x03 = 6 -- STORE_VAR      (int 4, type 1)
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
lenOp 0x10 = 1 -- INDEX          ()
lenOp _ = 0

-- * ---------------------------------------------- BINARY ----------------------------------------------

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

binaryOpCall 43 ((_, MyFloat y) : (_, MyFloat x) : xs) = (FloatType, MyFloat (x + y)) : xs
binaryOpCall 45 ((_, MyFloat y) : (_, MyFloat x) : xs) = (FloatType, MyFloat (x - y)) : xs
binaryOpCall 42 ((_, MyFloat y) : (_, MyFloat x) : xs) = (FloatType, MyFloat (x * y)) : xs
binaryOpCall 47 ((_, MyFloat y) : (_, MyFloat x) : xs) = (FloatType, MyFloat (x / y)) : xs
-- todo
-- binaryOpCall 37 ((_, MyFloat y) : (_, MyFloat x) : xs) = (FloatType, MyFloat (x `mod'` y)) : xs -- no mod for float
-- binaryOpCall 38 ((_, MyFloat y) : (_, MyFloat x) : xs) = (FloatType, MyFloat (x .&. y)) : xs
-- binaryOpCall 124 ((_, MyFloat y) : (_, MyFloat x) : xs) = (FloatType, MyFloat (x .|. y)) : xs
-- binaryOpCall 94 ((_, MyFloat y) : (_, MyFloat x) : xs) = (FloatType, MyFloat (x `xor` y)) : xs

binaryOpCall 43 ((_, MyChar y) : (_, MyChar x) : xs) = (CharType, MyChar (intToChar (ord x + ord y))) : xs
binaryOpCall 45 ((_, MyChar y) : (_, MyChar x) : xs) = (CharType, MyChar (intToChar (ord x - ord y))) : xs
binaryOpCall 42 ((_, MyChar y) : (_, MyChar x) : xs) = (CharType, MyChar (intToChar (ord x * ord y))) : xs
binaryOpCall 47 ((_, MyChar y) : (_, MyChar x) : xs) = (CharType, MyChar (intToChar (ord x `div` ord y))) : xs
binaryOpCall 37 ((_, MyChar y) : (_, MyChar x) : xs) = (CharType, MyChar (intToChar (ord x `mod` ord y))) : xs
binaryOpCall 38 ((_, MyChar y) : (_, MyChar x) : xs) = (CharType, MyChar (intToChar (ord x .&. ord y))) : xs
binaryOpCall 124 ((_, MyChar y) : (_, MyChar x) : xs) = (CharType, MyChar (intToChar (ord x .|. ord y))) : xs
binaryOpCall 94 ((_, MyChar y) : (_, MyChar x) : xs) = (CharType, MyChar (intToChar (ord x `xor` ord y))) : xs

binaryOpCall 43 ((_, MyString y) : (_, MyString x) : xs) = (StringType, MyString (x ++ y)) : xs
binaryOpCall 43 ((_, MyString y) : (_, MyChar x) : xs) = (StringType, MyString (x : y)) : xs
binaryOpCall 43 ((_, MyChar y) : (_, MyString x) : xs) = (StringType, MyString (x ++ [y])) : xs

binaryOpCall 45 ((_, MyInt y) : (_, MyFloat x) : xs) = (FloatType, MyFloat (x - intToFloat y)) : xs
-- binaryOpCall 45 ((_, MyInt y) : (_, MyChar x) : xs) = (CharType, MyChar (intToChar (x - ord y))) : xs -- todo

binaryOpCall 45 ((_, MyFloat y) : (_, MyInt x) : xs) = (FloatType, MyFloat (intToFloat x - y)) : xs
binaryOpCall 45 ((_, MyFloat y) : (_, MyChar x) : xs) = (FloatType, MyFloat (intToFloat (ord x) - y)) : xs

binaryOpCall 45 ((_, MyChar y) : (_, MyInt x) : xs) = (CharType, MyChar (intToChar (ord y - x))) : xs
binaryOpCall 45 ((_, MyChar y) : (_, MyFloat x) : xs) = (FloatType, MyFloat (x - intToFloat (ord y))) : xs

binaryOpCall _ stack = stack  -- Default case, no operation for other Word8 values
-- maybe & or | ?


-- * ---------------------------------------------- UNARY ----------------------------------------------


unaryOpCall :: Word8 -> StackTable -> StackTable
-- "-"
unaryOpCall 45 ((_, MyInt x) : xs) = (IntType, MyInt (-x)) : xs
unaryOpCall 45 ((_, MyFloat x) : xs) = (FloatType, MyFloat (-x)) : xs
unaryOpCall 45 ((_, MyChar x) : xs) = (CharType, MyChar (intToChar (-ord x))) : xs
-- "!"
unaryOpCall 33 ((_, MyInt x) : xs) = (IntType, MyInt (if x == 0 then 1 else 0)) : xs
unaryOpCall 33 ((_, MyFloat x) : xs) = (IntType, MyInt (if x == 0 then 1 else 0)) : xs
unaryOpCall 33 ((_, MyChar x) : xs) = (IntType, MyInt (if x == '\0' then 1 else 0)) : xs
unaryOpCall _ stack = stack  -- Default case, no operation for other Word8 values

-- * ---------------------------------------------- COMPARE ----------------------------------------------

-- 60 <
-- 62 >
-- 61 ==
-- 97 <=
-- 98 >=
-- 33 !=
compareOpCall :: Word8 -> StackTable -> StackTable
-- * Int
compareOpCall 60 ((_, MyInt y) : (_, MyInt x) : xs) =
    trace ("stack : top = " ++ show y ++ " > x = " ++ show x) ((if x < y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 62 ((_, MyInt y) : (_, MyInt x) : xs) =
    trace ("stack : top = " ++ show y ++ " < x = " ++ show x) ((if x > y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 61 ((_, MyInt y) : (_, MyInt x) : xs) =
    trace ("stack : top = " ++ show y ++ " == x = " ++ show x) ((if x == y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 97 ((_, MyInt y) : (_, MyInt x) : xs) =
    trace ("stack : top = " ++ show y ++ " <= x = " ++ show x) ((if x <= y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 98 ((_, MyInt y) : (_, MyInt x) : xs) =
    trace ("stack : top = " ++ show y ++ " >= x = " ++ show x) ((if x >= y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 33 ((_, MyInt y) : (_, MyInt x) : xs) =
    trace ("stack : top = " ++ show y ++ " != x = " ++ show x) ((if x /= y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
-- * Float
compareOpCall 60 ((_, MyFloat y) : (_, MyFloat x) : xs) =
    trace ("stack : top = " ++ show y ++ " > x = " ++ show x) ((if x < y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 62 ((_, MyFloat y) : (_, MyFloat x) : xs) =
    trace ("stack : top = " ++ show y ++ " < x = " ++ show x) ((if x > y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 61 ((_, MyFloat y) : (_, MyFloat x) : xs) =
    trace ("stack : top = " ++ show y ++ " == x = " ++ show x) ((if x == y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 97 ((_, MyFloat y) : (_, MyFloat x) : xs) =
    trace ("stack : top = " ++ show y ++ " <= x = " ++ show x) ((if x <= y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 98 ((_, MyFloat y) : (_, MyFloat x) : xs) =
    trace ("stack : top = " ++ show y ++ " >= x = " ++ show x) ((if x >= y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 33 ((_, MyFloat y) : (_, MyFloat x) : xs) =
    trace ("stack : top = " ++ show y ++ " != x = " ++ show x) ((if x /= y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
-- * Char
compareOpCall 60 ((_, MyChar y) : (_, MyChar x) : xs) =
    trace ("stack : top = " ++ show y ++ " > x = " ++ show x) ((if x < y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 62 ((_, MyChar y) : (_, MyChar x) : xs) =
    trace ("stack : top = " ++ show y ++ " < x = " ++ show x) ((if x > y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 61 ((_, MyChar y) : (_, MyChar x) : xs) =
    trace ("stack : top = " ++ show y ++ " == x = " ++ show x) ((if x == y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 97 ((_, MyChar y) : (_, MyChar x) : xs) =
    trace ("stack : top = " ++ show y ++ " <= x = " ++ show x) ((if x <= y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 98 ((_, MyChar y) : (_, MyChar x) : xs) =
    trace ("stack : top = " ++ show y ++ " >= x = " ++ show x) ((if x >= y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)
compareOpCall 33 ((_, MyChar y) : (_, MyChar x) : xs) =
    trace ("stack : top = " ++ show y ++ " != x = " ++ show x) ((if x /= y then (IntType, MyInt 1) else (IntType, MyInt 0)) : xs)

compareOpCall x stack = trace ("ERROR COMPARE OP : " ++ show x ++ " | stack : " ++ show stack) stack


-- * ---------------------------------------------- VARIABLE ----------------------------------------------

getFromVariable :: VariableId -> VariableTable -> VariableElement
getFromVariable varId ((n, _, e):xs)
    | varId == n = e
    | otherwise = getFromVariable varId xs
getFromVariable _ _ = trace "ERROR GET FROM VARIABLE" (MyInt 0)


toNewVariable :: VariableId -> VariableType -> VariableType -> VariableElement -> VariableEntry
-- * same type
toNewVariable varId IntType IntType (MyInt x) = trace ("int to int " ++ show x) $ (varId, IntType, MyInt x)
toNewVariable varId StringType StringType (MyString x) = trace ("string to string " ++ show x) $ (varId, StringType, MyString x)
toNewVariable varId CharType CharType (MyChar x) = trace ("char to char " ++ show x) $ (varId, CharType, MyChar x)
toNewVariable varId FloatType FloatType (MyFloat x) = trace ("float to float " ++ show x) $ (varId, FloatType, MyFloat x)
-- * convert
toNewVariable varId IntType FloatType (MyFloat x) = trace ("int to float " ++ show x) $ (varId, IntType, MyInt (round x))
toNewVariable varId FloatType IntType (MyInt x) = trace ("float to int " ++ show x) $ (varId, FloatType, MyFloat (intToFloat x))
toNewVariable varId IntType CharType (MyChar x) = trace ("int to char " ++ show x) $ (varId, IntType, MyInt (ord x))
toNewVariable varId CharType IntType (MyInt x) = trace ("char to int " ++ show x) $ (varId, CharType, MyChar (intToChar x))
toNewVariable varId FloatType CharType (MyChar x) = trace ("float to char " ++ show x) $ (varId, FloatType, MyFloat (intToFloat (ord x)))
toNewVariable varId CharType FloatType (MyFloat x) = trace ("char to float " ++ show x) $ (varId, CharType, MyChar (intToChar (round x)))
-- todo : string to int, float, char
toNewVariable varId CharType CharType (MyInt x) = trace ("int to char " ++ show x) $ (varId, CharType, MyChar (intToChar x))
toNewVariable varId IntType IntType (MyChar x) = trace ("char to int " ++ show x) $ (varId, IntType, MyInt (ord x))
toNewVariable a b c d = error ("ERROR TO NEW VARIABLE " ++ show a ++ " | " ++ show b ++ " | " ++ show c ++ " | " ++ show d)


new_VariableTable :: VariableId -> VariableType -> VariableElement -> VariableTable
new_VariableTable varId IntType (MyInt x) = [(varId, IntType, MyInt x)]
new_VariableTable varId StringType (MyString x) = [(varId, StringType, MyString x)]
new_VariableTable varId CharType (MyChar x) = [(varId, CharType, MyChar x)]
new_VariableTable varId FloatType (MyFloat x) = [(varId, FloatType, MyFloat x)]
new_VariableTable varId IntType (MyFloat x) = [(varId, IntType, MyInt (round x))]
new_VariableTable varId FloatType (MyInt x) = [(varId, FloatType, MyFloat (intToFloat x))]
new_VariableTable varId IntType (MyChar x) = [(varId, IntType, MyInt (ord x))]
new_VariableTable varId CharType (MyInt x) = [(varId, CharType, MyChar (intToChar x))]
new_VariableTable varId FloatType (MyChar x) = [(varId, FloatType, MyFloat (intToFloat (ord x)))]
new_VariableTable varId CharType (MyFloat x) = [(varId, CharType, MyChar (intToChar (round x)))]
new_VariableTable a b c = error ("ERROR NEW VARIABLE TABLE " ++ show a ++ " | " ++ show b ++ " | " ++ show c)


--                  id          type expected   new value               table    -> new table
updateVariable :: VariableId -> VariableType -> VariableElement -> VariableTable -> VariableTable
updateVariable varId varType element [] = new_VariableTable varId varType element
updateVariable varId varType element ((n, t, e):xs)
    | varId == n = toNewVariable varId varType t element : xs
    | otherwise = trace ("n = " ++ show n ++ " | t = " ++ show t ++ " | e = " ++ show e) $ (n, t, e) : updateVariable varId varType element xs


-- * ---------------------------------------------- STACK ----------------------------------------------

getLastIntFromStack :: StackTable -> Int
getLastIntFromStack [] = 0
getLastIntFromStack ((IntType, MyInt x):_) = x
getLastIntFromStack ((_, _):xs) = trace "Not an int, go next in stack" $ getLastIntFromStack xs

deleteLastValueFromStack :: StackTable -> StackTable
deleteLastValueFromStack [] = []
deleteLastValueFromStack ((IntType, _):xs) = xs
deleteLastValueFromStack ((FloatType, _):xs) = xs
deleteLastValueFromStack ((CharType, _):xs) = xs
deleteLastValueFromStack ((StringType, _):xs) = xs
deleteLastValueFromStack (x:xs) = x : deleteLastValueFromStack xs

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

-- * ---------------------------------------------- BYTECODE ----------------------------------------------

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

-- * ---------------------------------------------- STACK ----------------------------------------------

toStringFromHere :: [Word8] -> String
toStringFromHere [] = []
toStringFromHere (0x00:0x00:0x00:0x00:_) = []
toStringFromHere (a:b:c:d:xs) = intToChar (bytesToInt [a, b, c, d]) : toStringFromHere xs
toStringFromHere _ = trace "ERROR TO STRING FROM HERE" []

word8withAdresstoString :: [Word8] -> Int -> String
word8withAdresstoString [] _ = trace "EARLY END OF STRING" []
word8withAdresstoString (x:xs) 0 = toStringFromHere (x:xs)
word8withAdresstoString (_:xs) n = word8withAdresstoString xs (n - 1)

loadConst :: [Word8] -> Int -> StackEntry
loadConst (a:b:c:d:0x01:_) _ = (IntType, MyInt (bytesToInt [a, b, c, d]))
loadConst (a:b:c:d:0x02:xs) pc = (StringType, MyString (word8withAdresstoString xs ((bytesToInt [a, b, c, d]) - pc)))
loadConst (a:b:c:d:0x04:_) _ = (FloatType, MyFloat   (intToFloat (bytesToInt [a, b, c, d])))
loadConst (a:b:c:d:0x05:_) _ = (AddressType, MyInt (bytesToInt [a, b, c, d]))
loadConst (a:b:c:d:0x06:_) _ = (CharType, MyChar     (intToChar     (bytesToInt [a, b, c, d])))
loadConst _ _ = trace "ERROR LOAD CONST" (IntType, MyInt 0)


-- * ---------------------------------------------- PRINT ----------------------------------------------

printValueInStack :: StackEntry -> String
printValueInStack (IntType, MyInt x) = show x
printValueInStack (FloatType, MyFloat x) = show x
printValueInStack (CharType, MyChar x) = [x]
printValueInStack (AddressType, MyInt x) = show x
printValueInStack (StringType, MyString x) = trace ("string : " ++ show x) x
printValueInStack x = show x


-- * ---------------------------------------------- GET ----------------------------------------------

getNthValue :: Int -> [Word8] -> Word8
getNthValue 0 (x:_) = x
getNthValue n (_:xs) = (getNthValue (n - 1) xs)
getNthValue _ _ = trace "ERROR GET NTH VALUE" 0

getTypeFromValue :: Word8 -> VariableType
getTypeFromValue 0x01 = IntType
getTypeFromValue 0x02 = StringType
getTypeFromValue 0x03 = BoolType -- didn't exist
getTypeFromValue 0x04 = FloatType
getTypeFromValue 0x05 = AddressType
getTypeFromValue 0x06 = CharType
getTypeFromValue _ = trace "ERROR GET TYPE FROM VALUE" IntType

getVariableElementTypeFromStack :: StackTable -> VariableElement
getVariableElementTypeFromStack ((AddressType, _):xs) = getVariableElementTypeFromStack xs
getVariableElementTypeFromStack ((_, MyInt x):_) = MyInt x
getVariableElementTypeFromStack ((_, MyString x):_) = MyString x
getVariableElementTypeFromStack ((_, MyBool x):_) = MyBool x -- didn't exist
getVariableElementTypeFromStack ((_, MyFloat x):_) = MyFloat x
getVariableElementTypeFromStack ((_, MyChar x):_) = MyChar x
getVariableElementTypeFromStack _ = trace "Not a variable element, go next in stack" (MyInt 0)

-- * ---------------------------------------------- GET INDEX ----------------------------------------------

getCharIfOutOfBound :: Int -> String -> Char
getCharIfOutOfBound x y
    | x == -1 = y !! (length y - 1)
    | x < 0 = '\0'
    | x >= length y = '\0'
    | otherwise = y !! x

getIndex :: StackTable -> StackTable
getIndex ((IntType, MyInt x) : (StringType, MyString y) : xs) = (CharType, MyChar (getCharIfOutOfBound x y)) : xs
getIndex x = trace "ERROR GET INDEX" x

-- * ---------------------------------------------- EVAL ----------------------------------------------

-- ? stack is global, JUMP_NEW_SCOPE is useless ?
--           opcode   values    stack     PC    VariableTable    (new_stack, new_pc, new_VariableTable)
evalValue :: Word8 -> [Word8] -> StackTable -> Int -> VariableTable -> IO (StackTable, Int, VariableTable)
evalValue 0x01 values stack pc table = trace ("LOAD_CONST "    ++ show (bytesToInt values))        return (loadConst values (pc + lenOp 0x01) : stack, pc + lenOp 0x01, table)
evalValue 0x02 values stack pc table = trace ("LOAD_VAR "      ++ show (bytesToInt values) ++ " type: " ++ show (getTypeFromValue (getNthValue 4 values))) return (((getTypeFromValue (getNthValue 4 values)), (getFromVariable (bytesToInt values) table)) : stack, pc + lenOp 0x02, table)
evalValue 0x03 values stack pc table = trace ("STORE_VAR "     ++ show (bytesToInt values))        return (deleteLastValueFromStack stack, pc + lenOp 0x03, updateVariable (bytesToInt values) (getTypeFromValue (getNthValue 4 values)) (getVariableElementTypeFromStack stack) table)
evalValue 0x04 values stack pc table = trace ("BINARY_OP "     ++ show (word8ToInt (head values))) return (binaryOpCall (head values) stack, pc + lenOp 0x04, table)
evalValue 0x05 values stack pc table = trace ("UNARY_OP "      ++ show (word8ToInt (head values))) return (unaryOpCall (head values) stack, pc + lenOp 0x05, table)
evalValue 0x06 values stack pc table = trace ("COMPARE_OP "    ++ show (word8ToInt (head values))) return (compareOpCall (head values) stack, pc + lenOp 0x06, table)
evalValue 0x07 values stack pc table = trace ("JUMP_IF_TRUE "  ++ show (bytesToInt values))        (if (getLastIntFromStack stack) /= 0 then return(deleteLastValueFromStack stack, (bytesToInt values), table) else return(deleteLastValueFromStack stack, pc + lenOp 0x07, table))
evalValue 0x08 values stack pc table = trace ("JUMP_IF_FALSE " ++ show (bytesToInt values))        (if (getLastIntFromStack stack) == 0 then return(deleteLastValueFromStack stack, (bytesToInt values), table) else return(deleteLastValueFromStack stack, pc + lenOp 0x08, table))
evalValue 0x09 values stack _ table = trace ("JUMP "          ++ show (bytesToInt values))         return(stack, bytesToInt values, table)
evalValue 0x0A values stack _ table = trace  ("JUMP_NEW_SCOPE " ++ show (bytesToInt values))       return(stack, bytesToInt values, table)
evalValue 0x0B _ stack pc table = trace  "POP "                                                    return(tail stack, pc + lenOp 0x0B, table)
evalValue 0x0C _ (s:stack) pc table = trace  "DUP "                                                return(s : s : stack, pc + lenOp 0x0C, table)
-- * x == 1, print -- x == 60, exit
evalValue 0x0D (1:_) (x:xs) pc table = do
    putStr (printValueInStack x)
    return (xs, pc + lenOp 0x0D, table)
evalValue 0x0D (60:_) _ _ _ = trace "EXIT"                                                         return ([], -1, [])
evalValue 0x0E _ stack _ table = trace  "RETURN "                                                  return (deleteUntilAddressExceptOne stack 0, getLastAddressFromStack stack, table)
evalValue 0x0F _ stack pc table = trace "LOAD_PC "                                                 return (((AddressType, (MyInt (pc + lenOp 0x0F + lenOp 0x0A))) : stack), pc + lenOp 0x0F, table) -- pc + LOAD_PC + JUMP_NEW_SCOPE
evalValue 0x10 _ stack pc table = trace "INDEX "                                                   return (getIndex stack, pc + lenOp 0x10, table)
evalValue a b c d e = trace ("Unknown opcode: " ++ show a ++ " | values: " ++ show b ++ " | stack: " ++ show c ++ " | pc: " ++ show d ++ " | table: " ++ show e) return ([], -1, [])


-- * ---------------------------------------------- EVAL EACH ----------------------------------------------



-- ? we have two bytecodes lists because if we move forward in the list, we can't go back
--              bytecodes  bytecodes  stack      PC   VariableTable    -> stack
evalEachValue :: [Word8] -> [Word8] -> StackTable -> Int -> [VariableTable] -> IO StackTable
evalEachValue _ [] stack _ _ = return stack
evalEachValue bytecodes (x:xs) stack pc tables = do
    result <- if null tables
              then evalValue x xs stack pc []
              else evalValue x xs stack pc (head tables)
    let (new_stack, new_pc, new_table) = result
    let debugInfo = "pc = " ++ show pc ++ " | stack = " ++ show new_stack ++ " | next pc = " ++ show new_pc ++ " | table = " ++ show new_table
    if new_pc == -1 then
        return stack
    else
        if x == 0x0A then
            trace debugInfo $ evalEachValue bytecodes (drop new_pc bytecodes) new_stack new_pc ([] : new_table : (tail tables))
        else if x == 0x0E then
            trace debugInfo $ evalEachValue bytecodes (drop new_pc bytecodes) new_stack new_pc (tail tables)
        else
            trace debugInfo $ evalEachValue bytecodes (drop new_pc bytecodes) new_stack new_pc (new_table : (tail tables))


byteStringToWord8List :: BS.ByteString -> [Word8]
byteStringToWord8List = unpack

checkMagicNumber :: [Word8] -> Bool
checkMagicNumber [0x7a, 0x69, 0x7a, 0x69] = True
checkMagicNumber _ = False
