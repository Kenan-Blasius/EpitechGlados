module BytecodeToBinary where

import Types
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Control.Monad.State

-- * --
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

toHexaInt :: Int -> [Word8]
toHexaInt x = [fromIntegral x]

toHexaString :: String -> [Word8]
toHexaString x = map (fromIntegral . ord) x

getLengthOfOperation :: Bytecode -> Int
getLengthOfOperation (LoadConst _) = 2
getLengthOfOperation (LoadVar _) = 2
getLengthOfOperation (StoreVar _) = 2
getLengthOfOperation (BinaryOp _) = 2
getLengthOfOperation (UnaryOp _) = 2
getLengthOfOperation (CompareOp _) = 2
getLengthOfOperation (JumpIfTrue _) = 2
getLengthOfOperation (JumpIfFalse _) = 2
getLengthOfOperation (Jump _) = 2
getLengthOfOperation Pop = 1
getLengthOfOperation Dup = 1
getLengthOfOperation (Call _) = 2
getLengthOfOperation Return = 1
getLengthOfOperation (BuildList _) = 2
getLengthOfOperation Index = 1
getLengthOfOperation (Attribute _) = 2
getLengthOfOperation (CreateObject _) = 2


sumOfnNextBytes :: [Bytecode] -> Int -> Int
sumOfnNextBytes [] _ = 0
sumOfnNextBytes _ 0 = 0
sumOfnNextBytes (x:xs) i = getLengthOfOperation x + sumOfnNextBytes xs (i - 1)

toHexaInstruction :: Bytecode -> [Bytecode] -> State Int [Word8]
toHexaInstruction (LoadConst x) next =    trackBytes (getLengthOfOperation (LoadConst x))    >> return (0x01 : toHexaInt x)
toHexaInstruction (LoadVar x) next =      trackBytes (getLengthOfOperation (LoadVar x))      >> return (0x02 : toHexaString x)
toHexaInstruction (StoreVar x) next =     trackBytes (getLengthOfOperation (StoreVar x))     >> return (0x03 : toHexaString x)
toHexaInstruction (BinaryOp x) next =     trackBytes (getLengthOfOperation (BinaryOp x))     >> return (0x04 : toHexaString x)
toHexaInstruction (UnaryOp x) next =      trackBytes (getLengthOfOperation (UnaryOp x))      >> return (0x05 : toHexaString x)
toHexaInstruction (CompareOp x) next =    trackBytes (getLengthOfOperation (CompareOp x))    >> return (0x06 : toHexaString x)
toHexaInstruction (JumpIfTrue x) next = do
    currentBytes <- get
    trackBytes (getLengthOfOperation (JumpIfTrue x)) >> return (0x07 : toHexaInt (currentBytes + (sumOfnNextBytes next x) + 2))
toHexaInstruction (JumpIfFalse x) next = do
    currentBytes <- get
    trackBytes (getLengthOfOperation (JumpIfFalse x)) >> return (0x08 : toHexaInt (currentBytes + (sumOfnNextBytes next x) + 2))
toHexaInstruction (Jump x) next = do
    currentBytes <- get
    trackBytes (getLengthOfOperation (Jump x)) >> return (0x09 : toHexaInt (currentBytes + (sumOfnNextBytes next x) + 2))
toHexaInstruction Pop next =              trackBytes (getLengthOfOperation Pop)              >> return [0x0A]
toHexaInstruction Dup next =              trackBytes (getLengthOfOperation Dup)              >> return [0x0B]
toHexaInstruction (Call x) next =         trackBytes (getLengthOfOperation (Call x))         >> return (0x0C : toHexaInt x)
toHexaInstruction Return next =           trackBytes (getLengthOfOperation Return)           >> return [0x0D]
toHexaInstruction (BuildList x) next =    trackBytes (getLengthOfOperation (BuildList x))    >> return (0x0E : toHexaInt x)
toHexaInstruction Index next =            trackBytes (getLengthOfOperation Index)            >> return [0x0F]
toHexaInstruction (Attribute x) next =    trackBytes (getLengthOfOperation (Attribute x))    >> return (0x10 : toHexaString x)
toHexaInstruction (CreateObject x) next = trackBytes (getLengthOfOperation (CreateObject x)) >> return (0x11 : toHexaInt x)

trackBytes :: Int -> State Int ()
trackBytes n = modify (+ n)


bytecodeToBytes :: [Bytecode] -> State Int [Word8]
bytecodeToBytes [] = return []
bytecodeToBytes (x:xs) = do
    currentBytes <- get
    trace ("Current bytes: " ++ show currentBytes) $ do
        bytes <- toHexaInstruction x xs
        rest <- bytecodeToBytes xs
        return (bytes ++ rest)


writeBytesToFile :: [Word8] -> FilePath -> IO ()
writeBytesToFile bytes filePath = BS.writeFile filePath (BS.pack bytes)

bytecodeToBinary :: [Bytecode] -> IO ()
bytecodeToBinary bytecode = do
    let (bytes, totalBytes) = runState (bytecodeToBytes bytecode) 0
    putStrLn $ "Total bytes written: " ++ show totalBytes ++ " bytes :" ++ show bytes
    writeBytesToFile bytes "file.bin"