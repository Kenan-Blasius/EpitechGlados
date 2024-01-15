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
-- JUMP_NEW_SCOPE  0x0A
-- POP             0x0B
-- DUP             0x0C
-- CALL            0x0D
-- RETURN          0x0E
-- LOAD_PC         0x0F
-- INDEX           0x10
-- SAVE_AT         0x11

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
getLengthOfOperation (LoadConst _ _) = 6 -- 1 for the opcode and 4 for the   (int)  and 1 for the type
getLengthOfOperation (LoadVar _ _) = 6   -- 1 for the opcode and 4 for the (var id) and 1 for the type
getLengthOfOperation (StoreVar _ _) = 6  -- 1 for the opcode and 4 for the (var id) and 1 for the type
getLengthOfOperation (LoadVarBefore _ _) = 6  -- it's still with the name of the variable, it's an id in the bytecode
getLengthOfOperation (StoreVarBefore _ _) = 6 -- it's still with the name of the variable, it's an id in the bytecode
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
getLengthOfOperation (FunEntryPoint _ _) = 0 -- because it's a reference, it's removed from the bytecode
getLengthOfOperation (CallUserFun _) = 5 -- because will be remplace by a (JumpNewScope)
getLengthOfOperation LoadPC = 1
getLengthOfOperation (StringToSave _) = 0 -- because it exist only in the compiler, to save the string in the binary file
getLengthOfOperation Index = 1
getLengthOfOperation SaveAt = 1

dataTypeToByte :: DataType -> Word8
dataTypeToByte IntType = 0x01
dataTypeToByte StringType = 0x02
dataTypeToByte BoolType = 0x03
dataTypeToByte FloatType = 0x04
dataTypeToByte VoidType = 0x05
dataTypeToByte CharType = 0x06
dataTypeToByte FunType = 0x07
dataTypeToByte UnknownType = trace "ERROR ERROR /!\\: dataTypeToByte: UnknownType" 0x00


-- * ------------------------------------------ JUMP ---------------------------------------------- * --

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
findJumpRef ((JumpRef x) : xs) jumpId pos = if jumpId == x then pos else findJumpRef xs jumpId (pos + getLengthOfOperation (JumpRef x))
findJumpRef (x:xs) jumpId pos = findJumpRef xs jumpId (pos + getLengthOfOperation x)


--                 bytecode  -> jumpId -> nmb_jmp
remplaceAllJump :: [Bytecode] -> Int -> Int -> [Bytecode]
remplaceAllJump bytecode jumpId nmb_jmp | jumpId > nmb_jmp = bytecode
remplaceAllJump bytecode jumpId nmb_jmp = remplaceAllJump (remplaceJumpRef bytecode jumpId (findJumpRef bytecode jumpId sizeOfHeader)) (jumpId + 1) nmb_jmp

-- * ------------------------------------------ FUNCTION CALL ------------------------------------- * --

getFunctData :: [Bytecode] -> Int -> [(Int, String)]
getFunctData [] _ = []
getFunctData (FunEntryPoint x _ : xs) pos = (pos, x) : getFunctData xs pos
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
findPosMain (FunEntryPoint "main" _ : _) pos = pos
findPosMain (x:xs) pos = (findPosMain xs (pos + getLengthOfOperation x))

dispAllBytecode :: [Bytecode] -> Int -> IO ()
dispAllBytecode [] _ = return ()
dispAllBytecode (x:xs) pos = trace (show pos ++ " " ++ show x) (dispAllBytecode xs (pos + getLengthOfOperation x))


-- * ------------------------------------------ INSTRUCTION --------------------------------------- * --

--                  cur_instr  -> bytes
toHexaInstruction :: Bytecode -> [Word8]
toHexaInstruction (LoadConst x t) =   (0x01 : int32_ToBytes x ++ [dataTypeToByte t])
toHexaInstruction (LoadVarBefore _ _) =     error "Error: toHexaInstruction: LoadVarBefore" -- it's a reference, it's removed from the bytecode
toHexaInstruction (StoreVarBefore _ _) =    error "Error: toHexaInstruction: StoreVarBefore" -- it's a reference, it's removed from the bytecode
toHexaInstruction (LoadVar x t) =     (0x02 : int32_ToBytes x ++ [dataTypeToByte t])
toHexaInstruction (StoreVar x t) =    (0x03 : int32_ToBytes x ++ [dataTypeToByte t])
toHexaInstruction (BinaryOp x) =      (0x04 : [charToWord8 (head x)])
toHexaInstruction (UnaryOp x) =       (0x05 : [charToWord8 (head x)])
toHexaInstruction (CompareOp x) =     (0x06 : [charToWord8 (head x)])
toHexaInstruction (JumpIfTrue x) =    (0x07 : int32_ToBytes x)
toHexaInstruction (JumpIfFalse x) =   (0x08 : int32_ToBytes x)
toHexaInstruction (Jump x) =          (0x09 : int32_ToBytes x)
toHexaInstruction (JumpNewScope x) =  (0x0A : int32_ToBytes x)
toHexaInstruction (JumpRef _) =             error "Error: toHexaInstruction: JumpRef" -- it's a reference, it's removed from the bytecode
toHexaInstruction Pop =             [0x0B]
toHexaInstruction Dup =             [0x0C]
toHexaInstruction (Call x) =        (0x0D : int8_ToBytes x)
toHexaInstruction Return =          [0x0E]
toHexaInstruction (FunEntryPoint _ _) =     error "Error: toHexaInstruction: FunEntryPoint" -- it's a reference, it's removed from the bytecode
toHexaInstruction (CallUserFun _) =         error "Error: toHexaInstruction: CallUserFun" -- it's a reference, it's removed from the bytecode
toHexaInstruction LoadPC =          [0x0F]
toHexaInstruction Index =           [0x10]
toHexaInstruction SaveAt =          [0x11]
toHexaInstruction x = error ("Error: toHexaInstruction: Unknown instruction: " ++ show x)

-- * ------------------------------------------ HEADER -------------------------------------------- * --

getHeader :: [Word8]
getHeader = [0x7a, 0x69, 0x7a, 0x69] ++ (toHexaString "This is the comment section\0")

--                 instrs     -> bytes
bytecodeToBytes :: [Bytecode] -> [Word8]
bytecodeToBytes [] = []
bytecodeToBytes (x:xs) = (toHexaInstruction x) ++ bytecodeToBytes xs

writeBytesToFile :: FilePath -> [Word8] -> IO ()
writeBytesToFile filePath bytes = BS.writeFile filePath (BS.pack bytes)

-- * ------------------------------------------ GET SCOPES VARIALES ------------------------------- * --

differentUnknowType :: DataType -> Bool
differentUnknowType UnknownType = False
differentUnknowType _ = True


-- check all StoreVar in a row
getVariables :: [Bytecode] -> Int -> ([(String, DataType, Int)], Int)
getVariables [] i = ([], i)
getVariables (StoreVarBefore x t : xs) i | differentUnknowType t = let (vars, _) = getVariables xs (i + 1) in ((x, t, i) : vars, i)
getVariables (FunEntryPoint _ _ : _) i = ([], i) -- we find another scope, so we stop
getVariables (_:xs) i = getVariables xs i


getScopesVariables :: [Bytecode] -> Int -> ([(String, DataType, [(String, DataType, Int)])], Int)
getScopesVariables [] id_local = ([], id_local)
getScopesVariables (FunEntryPoint x t : xs) id_local = do
    let (vars, id2) = getVariables xs id_local
    let (scopes, id3) = getScopesVariables xs id2
    ((x, t, vars) : scopes, id3)
getScopesVariables (_:xs) id_local = let (scopes, id2) = getScopesVariables xs id_local in (scopes, id2)


-- * ------------------------------------------ FILL TYPES ---------------------------------------- * --

remplaceType :: [(String, DataType, Int)] -> String -> DataType
remplaceType [] x = error ("Error: Unknown variable: " ++ show x)
remplaceType ((name, t, _) : _) x | name == x = t
remplaceType (_ : ys) x = remplaceType ys x

findId :: [(String, DataType, Int)] -> String -> Int
findId [] x = error ("Error: Unknown variable: " ++ show x)
findId ((name, _, id1) : _) x | name == x = id1
findId (_ : ys) x = findId ys x

-- ! StoreVar only for return function
lookVarInFunction :: [Bytecode] -> [(String, DataType, Int)] -> [Bytecode]
lookVarInFunction [] _ = []
lookVarInFunction (LoadVarBefore x _ : xs) vars = (LoadVar (findId vars x) (remplaceType vars x)) : lookVarInFunction xs vars
lookVarInFunction (FunEntryPoint x t : xs) _ = (FunEntryPoint x t : xs) -- we find another scope, so we stop
lookVarInFunction (x : xs) vars = x : lookVarInFunction xs vars

findWhichFunction :: String -> [(String, DataType, [(String, DataType, Int)])] -> [(String, DataType, Int)]
findWhichFunction x [] = error ("Error: No function to remplace: " ++ show x)
findWhichFunction x ((name, _, vars) : _) | name == x = vars
findWhichFunction x (_ : ys) = findWhichFunction x ys


fillTypesInFunctions :: [Bytecode] -> [(String, DataType, [(String, DataType, Int)])] -> [Bytecode]
fillTypesInFunctions [] _ = []
fillTypesInFunctions (FunEntryPoint x t : xs) scopes = (FunEntryPoint x t) : (fillTypesInFunctions (lookVarInFunction xs (findWhichFunction x scopes))) scopes
fillTypesInFunctions (x : xs) scopes = x : fillTypesInFunctions xs scopes

-- * ------------------------------------------ FILL TYPES USER FUNCTION ------------------------- * --

findWhichFunctionReturn :: String -> [(String, DataType, [(String, DataType, Int)])] -> DataType
findWhichFunctionReturn x [] = error ("Error: No function to remplace: " ++ show x)
findWhichFunctionReturn x ((name, t, _) : _) | name == x = t
findWhichFunctionReturn x (_ : ys) = findWhichFunctionReturn x ys

checkGoodTypeReturn :: [(String, DataType, Int)] -> String -> DataType -> DataType
checkGoodTypeReturn [] x t = error ("Error: No variable found: " ++ show x ++ " type: " ++ show t)
checkGoodTypeReturn ((name, t2, _) : _) x t | name == x && t2 == t = t
checkGoodTypeReturn (_ : ys) x t = checkGoodTypeReturn ys x t

remplaceNextStoreVar :: [Bytecode] -> [(String, DataType, Int)] -> DataType -> [Bytecode]
remplaceNextStoreVar [] _ _ = []
remplaceNextStoreVar (StoreVarBefore x _ : xs) scope t = (StoreVar (findId scope x) (checkGoodTypeReturn scope x t)) : remplaceNextStoreVar xs scope t
remplaceNextStoreVar x _ _ = x

fillTypesUserFunction :: [Bytecode] -> String -> [(String, DataType, [(String, DataType, Int)])] -> [Bytecode]
fillTypesUserFunction [] _ _ = []
fillTypesUserFunction (FunEntryPoint x t : xs) _ scopes = (FunEntryPoint x t) : (fillTypesUserFunction xs x scopes)
fillTypesUserFunction (CallUserFun x : xs) name scopes = (CallUserFun x) : (fillTypesUserFunction (remplaceNextStoreVar xs (findWhichFunction name scopes) (findWhichFunctionReturn x scopes)) name scopes)
fillTypesUserFunction (x : xs) name scopes = x : fillTypesUserFunction xs name scopes

-- todo check good variable send to function
-- todo check good type return
-- todo check good assignation type

-- * ------------------------------------------ FILL LAST TYPES ---------------------------------- * --

removeBeforeVariables :: [Bytecode] -> [(String, DataType, Int)] -> [Bytecode]
removeBeforeVariables [] _ = []
removeBeforeVariables (LoadVarBefore x _ : xs) vars = (LoadVar (findId vars x) (remplaceType vars x)) : removeBeforeVariables xs vars -- maybe remplaceType can be avoidable ?
removeBeforeVariables (StoreVarBefore x _ : xs) vars = (StoreVar (findId vars x) (remplaceType vars x)) : removeBeforeVariables xs vars -- maybe remplaceType can be avoidable ?
removeBeforeVariables (FunEntryPoint x t : xs) _ = (FunEntryPoint x t : xs) -- we find another scope, so we stop
removeBeforeVariables (x : xs) vars = x : removeBeforeVariables xs vars

fillLastTypes :: [Bytecode] -> [(String, DataType, [(String, DataType, Int)])] -> [Bytecode]
fillLastTypes [] _ = []
fillLastTypes (FunEntryPoint x t : xs) scopes = (FunEntryPoint x t) : (fillLastTypes (removeBeforeVariables xs (findWhichFunction x scopes)) scopes)
fillLastTypes (x : xs) scopes = x : fillLastTypes xs scopes

-- * ------------------------------------------ STRING -------------------------------------------- * --

getSizeOfBytecode :: [Bytecode] -> Int
getSizeOfBytecode [] = 0
getSizeOfBytecode (x : xs) = getLengthOfOperation x + getSizeOfBytecode xs

stringToBytes :: String -> [Word8]
stringToBytes [] = [0x00, 0x00, 0x00, 0x00]
stringToBytes (x:xs) = (int32_ToBytes (fromEnum x)) ++ stringToBytes xs

saveStringAtTheEnd :: [Bytecode] -> Int -> ([Bytecode], [Word8])
saveStringAtTheEnd [] _ = ([], [])
saveStringAtTheEnd (LoadConst _ StringType : StringToSave str : xs) size =
    let (bytecode, bytes) = saveStringAtTheEnd xs (size + 4 * ((length str) + 1)) in
    (LoadConst size StringType : bytecode,  stringToBytes str ++ bytes)
saveStringAtTheEnd (x : xs) size =
    let (bytecode, bytes) = saveStringAtTheEnd xs size in
    (x : bytecode, bytes)

-- * ------------------------------------------ MAIN ---------------------------------------------- * --

bytecodeToBinary :: [Bytecode] -> String -> IO ()
bytecodeToBinary bytecode filename = do
    let nmp_jmp = getNmbrOfJumps bytecode

    putStrLn "-- * ---------------------------- * --"
    dispAllBytecode bytecode sizeOfHeader
    putStrLn "-- * ---------------------------- * --"

    -- remplace all JumpRef by Jump
    let bytecode2 = remplaceAllJump bytecode 1 nmp_jmp -- 1 because the first jump is at 1
    let (scopes_variables, _) = getScopesVariables bytecode2 0 -- 0 because the variable id start at 0
    print ("scopes_variables")
    print (scopes_variables)

    -- fill types in functions
    let bytecode2_0 = fillTypesInFunctions bytecode2 scopes_variables
    -- fill types in user functions
    let bytecode2_1 = fillTypesUserFunction bytecode2_0 "" scopes_variables
    -- variables to theire real id
    let bytecode2_2 = fillLastTypes bytecode2_1 scopes_variables

    putStrLn "-- * ---------------------------- * --"
    -- remove all JumpRef
    let bytecode3 = filter (\x -> case x of JumpRef _ -> False; _ -> True) bytecode2_2
    let funct_data = getFunctData bytecode3 sizeOfHeader

    -- remplace all CallUserFun by JumpNewScope
    let bytecode4 = (remplaceAllFun bytecode3 funct_data)
    let posMain = findPosMain bytecode4 sizeOfHeader

    -- save all string at the end of the binary file
    let (bytecode4_1, bytes) = saveStringAtTheEnd bytecode4 ((getSizeOfBytecode bytecode4) + sizeOfHeader)

    if posMain == 0
    then
        error "Error: No main function found"
    else do
        -- add the first jump to the main function
        let bytecode5 = [(Jump posMain)] ++ filter (\x -> case x of FunEntryPoint _ _ -> False; _ -> True) bytecode4_1

        print ("bytecode:")
        dispAllBytecode bytecode5 (sizeOfHeader - 5)
        putStrLn "-- * ---------------------------- * --"

        print ("bytes:")
        print (bytes)

        let binary = getHeader ++ (bytecodeToBytes bytecode5) ++ bytes
        putStrLn "-- * ---------------------------- * --"
        print ("bytecode:")
        dispAllBytecode bytecode5 (sizeOfHeader - 5)
        putStrLn "-- * ---------------------------- * --"
        print ("binary")
        print (binary)
        writeBytesToFile (filename ++ ".bin") binary


-- ? store the strings at the end of the binary file



-- todo deadleaf + or - -> 0
-- todo deadleaf * or /  or % -> 1


-- handle functions without arguments


-- handle this case:
--     j = 4;
--     int j = 4;
