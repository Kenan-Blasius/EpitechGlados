module BytecodeToBinary (
    bytecodeToBinary
) where

import Types
import Data.Char
import Debug.Trace
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.Bits


sizeOfHeader :: Int
sizeOfHeader = 32 + 5 -- 32 for the header and 5 for the first jump

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

int8_ToBytes :: Int -> [Word8]
int8_ToBytes x = [fromIntegral x]

int32_ToBytes :: Int -> [Word8]
int32_ToBytes x = map fromIntegral [x .&. 0xFF, (x `shiftR` 8) .&. 0xFF, (x `shiftR` 16) .&. 0xFF, (x `shiftR` 24) .&. 0xFF]


toHexaString :: String -> [Word8]
toHexaString x = map (fromIntegral . ord) x

charToWord8 :: Char -> Word8
charToWord8 = fromIntegral . fromEnum


-- TODO variables names stored as id

getLengthOfOperation :: Bytecode -> Int
getLengthOfOperation (LoadConst _ _) = 6 -- 1 for the opcode and 4 for the int and 1 for the type
getLengthOfOperation (LoadVar _ _) = 3
getLengthOfOperation (StoreVar _ _) = 3 -- usually 8 bytes for a pointer
getLengthOfOperation (BinaryOp _) = 2
getLengthOfOperation (UnaryOp _) = 2
getLengthOfOperation (CompareOp _) = 2
getLengthOfOperation (JumpIfTrue _) = 5 -- should not append
getLengthOfOperation (JumpIfFalse _) = 5 -- should not append
getLengthOfOperation (Jump _) = 5 -- should not append
getLengthOfOperation (JumpNewScope _) = 5
getLengthOfOperation (JumpIfTrueBefore _) = 5
getLengthOfOperation (JumpIfFalseBefore _) = 5
getLengthOfOperation (JumpBefore _) = 5
getLengthOfOperation (JumpRef _) = 0 -- because it's a reference, it's removed from the bytecode
getLengthOfOperation Pop = 1
getLengthOfOperation Dup = 1
getLengthOfOperation (Call _) = 2
getLengthOfOperation Return = 1
getLengthOfOperation (FunEntryPoint _) = 0
getLengthOfOperation (CallUserFun _) = 5
getLengthOfOperation LoadPC = 1

dataTypeToByte :: DataType -> Word8
dataTypeToByte IntType = 0x01
dataTypeToByte StringType = 0x02
dataTypeToByte BoolType = 0x03
dataTypeToByte FloatType = 0x04
dataTypeToByte VoidType = 0x05


-- * ------------------------------------------ JUMP ------------------------------------------ * --

getNmbrOfJumps :: [Bytecode] -> Int
getNmbrOfJumps [] = 0
getNmbrOfJumps (x:xs) = case x of
    JumpRef _ -> 1 + getNmbrOfJumps xs
    _ -> getNmbrOfJumps xs

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
remplaceAllJump bytecode jumpId nmb_jmp = remplaceAllJump (remplaceJumpRef bytecode jumpId (findJumpRef bytecode jumpId sizeOfHeader)) (jumpId + 1) nmb_jmp

-- * ------------------------------------------ FUNCTION CALL ------------------------------------------ * --

getFunctData :: [Bytecode] -> Int -> [(Int, String)]
getFunctData [] _ = []
getFunctData (FunEntryPoint x : xs) pos = (pos, x) : getFunctData xs pos
getFunctData (x:xs) pos = getFunctData xs (pos + getLengthOfOperation x)

remplaceFunEntryPoint :: [Bytecode] -> (Int, String) -> [Bytecode]
remplaceFunEntryPoint [] _ = []
remplaceFunEntryPoint ((CallUserFun x) : xs) (pos, name) | x == name = (JumpNewScope pos : remplaceFunEntryPoint xs (pos, name))
remplaceFunEntryPoint (x:xs) fun_data = x : remplaceFunEntryPoint xs fun_data


--                 bytecode  -> (pos, name) ->      bytecode
remplaceAllFun :: [Bytecode] -> [(Int, String)] -> [Bytecode]
remplaceAllFun bytecode [] = bytecode
remplaceAllFun bytecode (x : xs) = (remplaceAllFun (remplaceFunEntryPoint bytecode x) xs)

findPosMain :: [Bytecode] -> Int -> Int
findPosMain [] _ = 0
findPosMain (FunEntryPoint "main" : _) pos = pos
findPosMain (x:xs) pos = (findPosMain xs (pos + getLengthOfOperation x))

dispAllBytecode :: [Bytecode] -> Int -> IO ()
dispAllBytecode [] _ = return ()
dispAllBytecode (x:xs) pos = trace (show pos ++ " " ++ show x) (dispAllBytecode xs (pos + getLengthOfOperation x))


-- * ------------------------------------------ INSTRUCTION ------------------------------------------ * --

--                  cur_instr  -> bytes
toHexaInstruction :: Bytecode -> [Word8]
toHexaInstruction (LoadConst x) =   (0x01 : int32_ToBytes x ++ [dataTypeToByte t])
toHexaInstruction (LoadVar x) =     (0x02 : toHexaString x ++ [dataTypeToByte t]) -- TODO as id
toHexaInstruction (StoreVar x) =    (0x03 : toHexaString x ++ [dataTypeToByte t])
toHexaInstruction (BinaryOp x) =    (0x04 : [charToWord8 (head x)])
toHexaInstruction (UnaryOp x) =     (0x05 : [charToWord8 (head x)])
toHexaInstruction (CompareOp x) =   (0x06 : [charToWord8 (head x)])
toHexaInstruction (JumpIfTrue x) =  (0x07 : int32_ToBytes x)
toHexaInstruction (JumpIfFalse x) = (0x08 : int32_ToBytes x)
toHexaInstruction (Jump x) =        (0x09 : int32_ToBytes x)
toHexaInstruction (JumpNewScope x) = (0x0A : int32_ToBytes x)
--  JumpRef should not append
toHexaInstruction Pop =             [0x0B]
toHexaInstruction Dup =             [0x0C]
toHexaInstruction (Call x) =        (0x0D : int8_ToBytes x)
toHexaInstruction Return =          [0x0E]
toHexaInstruction LoadPC =          [0x0F]
toHexaInstruction x = trace ("Error: toHexaInstruction: Unknown instruction" ++ show x) []

-- * ------------------------------------------ HEADER ------------------------------------------ * --

getHeader :: [Word8]
getHeader = [0x7a, 0x69, 0x7a, 0x69] ++ (toHexaString "This is the comment section\0")

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
    -- print ("bytecode3")
    -- print (bytecode3)
    let funct_data = getFunctData bytecode3 sizeOfHeader
    -- print ("funct_data")
    -- print (funct_data)
    let bytecode4 = (remplaceAllFun bytecode3 funct_data)
    -- print ("bytecode4")
    -- print (bytecode4)
    let posMain = findPosMain bytecode4 sizeOfHeader
    let bytecode5 = [(Jump posMain)] ++ filter (\x -> case x of FunEntryPoint _ -> False; _ -> True) bytecode4

    dispAllBytecode bytecode5 (sizeOfHeader - 5)
    print ("bytecode5")
    print (bytecode5)
    let binary = getHeader ++ (bytecodeToBytes bytecode5)
    print ("binary")
    print (binary)
    writeBytesToFile "file.bin" binary

-- todo 5 first bytes after header are a jmp to the main function (entry point)

-- ? maybe later also for store the string



-- todo deadleaf + or - -> 0
-- todo deadleaf * or /  or % -> 1