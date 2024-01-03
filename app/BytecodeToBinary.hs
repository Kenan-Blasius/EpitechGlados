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

-- data Bytecode = LoadConst Int
--               | LoadVar String
--               | StoreVar String
--               | BinaryOp String
--               | UnaryOp String
--               | CompareOp String
--               | JumpIfTrue Int
--               | JumpIfFalse Int
--               | Jump Int
--               | Pop
--               | Dup
--               | Call Int
--               | Return
--               | BuildList Int
--               | Index
--               | Attribute String
--               | CreateObject Int
--               deriving Eq

toHexaInt :: Int -> [Word8]
toHexaInt x = [fromIntegral x]

toHexaString :: String -> [Word8]
toHexaString x = map (fromIntegral . ord) x

-- to use somewhere
-- toHexaInstruction :: Bytecode -> [Word8]
-- toHexaInstruction (LoadConst x) = [0x01] ++ toHexaInt x
-- toHexaInstruction (LoadVar x) = [0x02] ++ toHexaString x
-- toHexaInstruction (StoreVar x) = [0x03] ++ toHexaString x
-- toHexaInstruction (BinaryOp x) = [0x04] ++ toHexaString x
-- toHexaInstruction (UnaryOp x) = [0x05] ++ toHexaString x
-- toHexaInstruction (CompareOp x) = [0x06] ++ toHexaString x
-- toHexaInstruction (JumpIfTrue x) = [0x07] ++ toHexaInt x
-- toHexaInstruction (JumpIfFalse x) = [0x08] ++ toHexaInt x
-- toHexaInstruction (Jump x) = [0x09] ++ toHexaInt x
-- toHexaInstruction Pop = [0x0A]
-- toHexaInstruction Dup = [0x0B]
-- toHexaInstruction (Call x) = [0x0C] ++ toHexaInt x
-- toHexaInstruction Return = [0x0D]
-- toHexaInstruction (BuildList x) = [0x0E] ++ toHexaInt x
-- toHexaInstruction Index = [0x0F]
-- toHexaInstruction (Attribute x) = [0x10] ++ toHexaString x
-- toHexaInstruction (CreateObject x) = [0x11] ++ toHexaInt x

toHexaInstruction :: Bytecode -> State Int [Word8]
toHexaInstruction (LoadConst x) = trackBytes 1 >> return (0x01 : toHexaInt x)
toHexaInstruction (LoadVar x) = trackBytes 1 >> return (0x02 : toHexaString x)
toHexaInstruction (StoreVar x) = trackBytes 1 >> return (0x03 : toHexaString x)
toHexaInstruction (BinaryOp x) = trackBytes 1 >> return (0x04 : toHexaString x)
toHexaInstruction (UnaryOp x) = trackBytes 1 >> return (0x05 : toHexaString x)
toHexaInstruction (CompareOp x) = trackBytes 1 >> return (0x06 : toHexaString x)
toHexaInstruction (JumpIfTrue x) = trackBytes 1 >> return (0x07 : toHexaInt (x + 1))
toHexaInstruction (JumpIfFalse x) = trackBytes 1 >> return (0x08 : toHexaInt (x + 1))
toHexaInstruction (Jump x) = trackBytes 1 >> return (0x09 : toHexaInt (x + 1))
toHexaInstruction Pop = trackBytes 1 >> return [0x0A]
toHexaInstruction Dup = trackBytes 1 >> return [0x0B]
toHexaInstruction (Call x) = trackBytes 1 >> return (0x0C : toHexaInt x)
toHexaInstruction Return = trackBytes 1 >> return [0x0D]
toHexaInstruction (BuildList x) = trackBytes 1 >> return (0x0E : toHexaInt x)
toHexaInstruction Index = trackBytes 1 >> return [0x0F]
toHexaInstruction (Attribute x) = trackBytes 1 >> return (0x10 : toHexaString x)
toHexaInstruction (CreateObject x) = trackBytes 1 >> return (0x11 : toHexaInt x)

trackBytes :: Int -> State Int ()
trackBytes n = modify (+ n)


-- Function to pretty print a list of bytecode instructions

-- bytecodeToBytes :: [Bytecode] -> [Word8]
-- bytecodeToBytes bytecode = concat $ map toHexaInstruction bytecode
bytecodeToBytes :: [Bytecode] -> ([Word8], Int)
bytecodeToBytes bytecode = runState (concat <$> mapM toHexaInstruction bytecode) 0


-- writeBytesToFile :: [Word8] -> FilePath -> IO ()
-- writeBytesToFile bytes filePath = BS.writeFile filePath (BS.pack bytes)
writeBytesToFile :: [Word8] -> FilePath -> IO ()
writeBytesToFile bytes filePath = BS.writeFile filePath (BS.pack bytes)





-- bytecodeToBinary :: [Bytecode] -> IO ()
-- bytecodeToBinary bytecode = do
--     -- writeFile "file.txt" (printBytecode bytecode)
--     writeBytesToFile (bytecodeToBytes bytecode) "file.bin"

bytecodeToBinary :: [Bytecode] -> IO ()
bytecodeToBinary bytecode = do
    let (bytes, totalBytes) = bytecodeToBytes bytecode
    putStrLn $ "Total bytes written: " ++ show totalBytes
    writeBytesToFile bytes "file.bin"
