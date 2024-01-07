module BytecodeToBinary (
    bytecodeToBinary
) where

import Types
import Data.Char
import Debug.Trace
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Bits

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

toHexaInt :: Int -> [Word8]
toHexaInt x = [fromIntegral x]

intToBytes :: Int -> [Word8]
intToBytes x = map fromIntegral [x .&. 0xFF, (x `shiftR` 8) .&. 0xFF, (x `shiftR` 16) .&. 0xFF, (x `shiftR` 24) .&. 0xFF]


toHexaString :: String -> [Word8]
toHexaString x = map (fromIntegral . ord) x

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . fromEnum

getNmbrOfJumps :: [Bytecode] -> Int
getNmbrOfJumps [] = 0
getNmbrOfJumps (x:xs) = case x of
    JumpRef _ -> 1 + getNmbrOfJumps xs
    _ -> getNmbrOfJumps xs

-- TODO variables names stored as id

getLengthOfOperation :: Bytecode -> Int
getLengthOfOperation (LoadConst _) = 5 -- 1 for the opcode and 4 for the int
getLengthOfOperation (LoadVar _) = 2
getLengthOfOperation (StoreVar _) = 2 -- usually 8 bytes for a pointer
getLengthOfOperation (BinaryOp _) = 2
getLengthOfOperation (UnaryOp _) = 2
getLengthOfOperation (CompareOp _) = 2
getLengthOfOperation (JumpIfTrue _) = 5 -- should not append
getLengthOfOperation (JumpIfFalse _) = 5 -- should not append
getLengthOfOperation (Jump _) = 5 -- should not append
getLengthOfOperation (JumpIfTrueBefore _) = 5
getLengthOfOperation (JumpIfFalseBefore _) = 5
getLengthOfOperation (JumpBefore _) = 5
getLengthOfOperation (JumpRef _) = 0 -- because it's a reference, it's removed from the bytecode
getLengthOfOperation Pop = 1
getLengthOfOperation Dup = 1
getLengthOfOperation (Call _) = 2
getLengthOfOperation Return = 1

--                 bytecode  -> id -> position
remplaceJumpRef :: [Bytecode] -> Int -> Int -> [Bytecode]
remplaceJumpRef [] _ _ = []
remplaceJumpRef ((JumpIfTrueBefore x) : xs) jumpId pos | jumpId == x = (JumpIfTrue pos : remplaceJumpRef xs jumpId pos)
remplaceJumpRef ((JumpIfFalseBefore x) : xs) jumpId pos | jumpId == x = (JumpIfFalse pos : remplaceJumpRef xs jumpId pos)
remplaceJumpRef ((JumpBefore x) : xs) jumpId pos | jumpId == x = (Jump pos : remplaceJumpRef xs jumpId pos)
remplaceJumpRef (x:xs) jumpId pos = x : remplaceJumpRef xs jumpId pos


--                bytecode -> jumpId -> position
findJumpRef :: [Bytecode] -> Int -> Int -> Int
findJumpRef [] _ _ = trace "Error: No JumpRef found" 0
findJumpRef ((JumpRef x) : xs) jumpId pos = if jumpId == x then
    trace ("findJumpRef: " ++ show x ++ " " ++ show jumpId ++ " " ++ show pos) pos
    else findJumpRef xs jumpId (pos + getLengthOfOperation (JumpRef x))
findJumpRef (x:xs) jumpId pos = findJumpRef xs jumpId (pos + getLengthOfOperation x)


--                 bytecode  -> jumpId -> nmb_jmp
remplaceAllJump :: [Bytecode] -> Int -> Int -> [Bytecode]
remplaceAllJump bytecode jumpId nmb_jmp | jumpId > nmb_jmp = bytecode
remplaceAllJump bytecode jumpId nmb_jmp = remplaceAllJump (remplaceJumpRef bytecode jumpId (findJumpRef bytecode jumpId 0)) (jumpId + 1) nmb_jmp

--                  cur_instr  -> bytes
toHexaInstruction :: Bytecode -> [Word8]
toHexaInstruction (LoadConst x) =   (0x01 : intToBytes x)
toHexaInstruction (LoadVar x) =     (0x02 : toHexaString x) -- TODO as id
toHexaInstruction (StoreVar x) =    (0x03 : toHexaString x)
toHexaInstruction (BinaryOp x) =    (0x04 : toHexaString x)
toHexaInstruction (UnaryOp x) =     (0x05 : toHexaString x)
toHexaInstruction (CompareOp x) =   (0x06 : [charToWord8 (x !! 0)])
toHexaInstruction (JumpIfTrue x) =  (0x07 : intToBytes x)
toHexaInstruction (JumpIfFalse x) = (0x08 : intToBytes x)
toHexaInstruction (Jump x) =        (0x09 : intToBytes x)
--  JumpRef should not append
toHexaInstruction Pop =             [0x0A]
toHexaInstruction Dup =             [0x0B]
toHexaInstruction (Call x) =        (0x0C : toHexaInt x)
toHexaInstruction Return =          [0x0D]
toHexaInstruction x = trace ("Error: toHexaInstruction: Unknown instruction" ++ show x) []

--                 instrs     -> bytes
bytecodeToBytes :: [Bytecode] -> [Word8]
bytecodeToBytes [] = []
bytecodeToBytes (x:xs) = (toHexaInstruction x) ++ bytecodeToBytes xs

writeBytesToFile :: FilePath -> [Word8] -> IO ()
writeBytesToFile filePath bytes = BS.writeFile filePath (BS.pack bytes)

bytecodeToBinary :: [Bytecode] -> IO ()
bytecodeToBinary bytecode = do
    let nmp_jmp = getNmbrOfJumps bytecode
    let bytecode2 = remplaceAllJump bytecode 1 nmp_jmp -- 1 because the first jump is at 1
    let bytecode3 = filter (\x -> case x of JumpRef _ -> False; _ -> True) bytecode2
    print (bytecode3)
    print (bytecodeToBytes bytecode3)
    writeBytesToFile "file.bin" (bytecodeToBytes bytecode3)

