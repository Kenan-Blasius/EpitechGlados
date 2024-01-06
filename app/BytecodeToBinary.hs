module BytecodeToBinary (
    bytecodeToBinary
) where

import Types
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

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . fromEnum

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

--            next_instrs   move_left -> bytes
sumOfnNextBytes :: [Bytecode] -> Int -> Int
sumOfnNextBytes [] _ = 0
sumOfnNextBytes _ 0 = 0
sumOfnNextBytes (x:xs) i = trace ("Sum of n next bytes: " ++ show i ++ " " ++ show (getLengthOfOperation x)) $
    getLengthOfOperation x + sumOfnNextBytes xs (i - 1)

--                 next_instrs   rev_instrs  size_all_instrs currentBytes move_size -> bytes
getPositionToJump :: [Bytecode] -> [Bytecode] -> Int -> Int -> Int -> Int
getPositionToJump next rev size_all_instr currentBytes move_size
    | move_size < 0 = trace ("Move size: " ++ show move_size ++ " currentBytes: " ++ show currentBytes ++ " instrs: " ++ show (drop (size_all_instr - (currentBytes + 1)) rev))
    $ currentBytes - (sumOfnNextBytes (drop (size_all_instr - (currentBytes + 1)) rev) (- move_size))
    | otherwise = trace ("Move size: " ++ show move_size ++ " currentBytes: " ++ show currentBytes ++ " instrs: " ++ show (take (currentBytes + 1) next))
         currentBytes + 2 + sumOfnNextBytes next move_size
--                      | 2 is for the size of the jump instruction

--                  cur_instr    next_instrs   rev_instrs  size_all_instrs -> bytes
toHexaInstruction :: Bytecode -> [Bytecode] -> [Bytecode] -> Int -> State Int [Word8]
toHexaInstruction (LoadConst x) _ _ _ =    trackBytes (getLengthOfOperation (LoadConst x))    >> return (0x01 : toHexaInt x)
toHexaInstruction (LoadVar x) _ _ _ =      trackBytes (getLengthOfOperation (LoadVar x))      >> return (0x02 : toHexaString x)
toHexaInstruction (StoreVar x) _ _ _ =     trackBytes (getLengthOfOperation (StoreVar x))     >> return (0x03 : toHexaString x)
toHexaInstruction (BinaryOp x) _ _ _ =     trackBytes (getLengthOfOperation (BinaryOp x))     >> return (0x04 : toHexaString x)
toHexaInstruction (UnaryOp x) _ _ _ =      trackBytes (getLengthOfOperation (UnaryOp x))      >> return (0x05 : toHexaString x)
toHexaInstruction (CompareOp x) _ _ _ =    trackBytes (getLengthOfOperation (CompareOp x))    >> return (0x06 : [charToWord8 (x !! 0)])
toHexaInstruction (JumpIfTrue x) next rev size_all = do
    currentBytes <- get
    trackBytes (getLengthOfOperation (JumpIfTrue x)) >> return (0x07 : toHexaInt (getPositionToJump next rev size_all currentBytes x))
toHexaInstruction (JumpIfFalse x) next rev size_all = do
    currentBytes <- get
    trackBytes (getLengthOfOperation (JumpIfFalse x)) >> return (0x08 : toHexaInt (getPositionToJump next rev size_all currentBytes x))
toHexaInstruction (Jump x) next rev size_all = do
    currentBytes <- get
    trackBytes (getLengthOfOperation (Jump x)) >> return (0x09 : toHexaInt (getPositionToJump next rev size_all currentBytes x))
toHexaInstruction Pop _ _ _ =              trackBytes (getLengthOfOperation Pop)              >> return [0x0A]
toHexaInstruction Dup _ _ _ =              trackBytes (getLengthOfOperation Dup)              >> return [0x0B]
toHexaInstruction (Call x) _ _ _ =         trackBytes (getLengthOfOperation (Call x))         >> return (0x0C : toHexaInt x)
toHexaInstruction Return _ _ _ =           trackBytes (getLengthOfOperation Return)           >> return [0x0D]
toHexaInstruction (BuildList x) _ _ _ =    trackBytes (getLengthOfOperation (BuildList x))    >> return (0x0E : toHexaInt x)
toHexaInstruction Index _ _ _ =            trackBytes (getLengthOfOperation Index)            >> return [0x0F]
toHexaInstruction (Attribute x) _ _ _ =    trackBytes (getLengthOfOperation (Attribute x))    >> return (0x10 : toHexaString x)
toHexaInstruction (CreateObject x) _ _ _ = trackBytes (getLengthOfOperation (CreateObject x)) >> return (0x11 : toHexaInt x)

trackBytes :: Int -> State Int ()
trackBytes n = modify (+ n)


--                 instrs      rev_instrs  size_all_instrs -> bytes
bytecodeToBytes :: [Bytecode] -> [Bytecode] -> Int -> State Int [Word8]
bytecodeToBytes [] _ _ = return []
bytecodeToBytes (x:xs) rev size_all_instr = do
    currentBytes <- get
    trace ("Current bytes: " ++ show currentBytes) $ do
        bytes <- toHexaInstruction x xs rev size_all_instr
        rest <- bytecodeToBytes xs rev size_all_instr
        return (bytes ++ rest)


writeBytesToFile :: [Word8] -> FilePath -> IO ()
writeBytesToFile bytes filePath = BS.writeFile filePath (BS.pack bytes)

bytecodeToBinary :: [Bytecode] -> IO ()
bytecodeToBinary bytecode = do
    let size_all_instr = trace ("Size of all instructions: " ++ show (sum (map getLengthOfOperation bytecode))) sum (map getLengthOfOperation bytecode)
    let (bytes, totalBytes) = runState (bytecodeToBytes bytecode (reverse bytecode) size_all_instr) 0
    putStrLn $ "Total bytes written: " ++ show totalBytes ++ " bytes :" ++ show bytes
    writeBytesToFile bytes "file.bin"
