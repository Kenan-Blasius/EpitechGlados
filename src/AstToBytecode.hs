module AstToBytecode (
    astToBytecode',
    astConditionToBytecode,
    valueSimpleToBytecode,
    astStoreValue,
) where

import Types(Bytecode(..), AST(..), DataType(..))
import Debug.Trace
import Unsafe.Coerce (unsafeCoerce)

-- TODO !var

valueSimpleToBytecode :: AST -> [Bytecode]
valueSimpleToBytecode (AST []) = []
valueSimpleToBytecode (AST (x:_)) =
    case x of
        IntAST y -> [LoadConst y IntType]
        SymbolAST y -> [LoadVarBefore y StringType]
        _ -> []
valueSimpleToBytecode _ = []


astConditionToBytecode :: AST -> [Bytecode]
astConditionToBytecode (AST []) = []
astConditionToBytecode (EqualAST            cond1 cond2) = (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp "=="]
astConditionToBytecode (LessThanAST         cond1 cond2) = (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp "<"]
astConditionToBytecode (GreaterThanAST      cond1 cond2) = (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp ">"]
astConditionToBytecode (LessThanEqualAST    cond1 cond2) = (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp "<="]
astConditionToBytecode (GreaterThanEqualAST cond1 cond2) = (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp ">="]
astConditionToBytecode (NotEqualAST         cond1 cond2) = (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp "!="]
astConditionToBytecode (AndAST cond1 cond2) = astConditionToBytecode cond1 ++ astConditionToBytecode cond2 ++ [BinaryOp "&&"]
astConditionToBytecode (OrAST cond1 cond2) = astConditionToBytecode cond1 ++ astConditionToBytecode cond2 ++ [BinaryOp "||"]
astConditionToBytecode x = trace ("astConditionToBytecode NO AST CONDITION NODE FOUND: " ++ show x) []


astStoreValue :: AST -> [Bytecode]
astStoreValue (AST [IntTypeAST, SymbolAST x]) = [StoreVarBefore x IntType]
astStoreValue (AST [StringTypeAST, SymbolAST x]) = [StoreVarBefore x StringType]
astStoreValue (AST [CharTypeAST, SymbolAST x]) = [StoreVarBefore x CharType]
astStoreValue (AST [FloatTypeAST, SymbolAST x]) = [StoreVarBefore x FloatType]
astStoreValue (AST [SymbolAST x]) = [StoreVarBefore x UnknownType] -- ! really unknown type ?
astStoreValue x = trace ("astStoreValue NO AST STORE NODE FOUND" ++ show x) $ []

astStoreArgs :: AST -> [Bytecode]
astStoreArgs DeadLeafAST = []
astStoreArgs (AST []) = []
astStoreArgs (AST (AST(x):xs)) = astStoreArgs (AST xs) ++ astStoreArgs (AST (x))
astStoreArgs x = astStoreValue x

getTypes :: AST -> DataType
getTypes (AST []) = error "ERROR getTypes empty"
getTypes (IntTypeAST) = IntType
getTypes (CharTypeAST) = CharType
getTypes (StringTypeAST) = StringType
getTypes (FloatTypeAST) = FloatType
getTypes (AST (x:_)) = getTypes x
getTypes x = error ("ERROR getTypes " ++ show x)


-- INFO: This function takes an AST and returns a list of Bytecode instructions
--       that can be executed by the VM.

-- todo get function even if no var in args

--             AST      id_jmp -> (AST bytecode id_jmp)
astToBytecode' :: AST -> Int -> (AST, [Bytecode], Int)
astToBytecode' (AST []) jmp = (AST [], [], jmp)

-- * System calls
astToBytecode' (AST [SymbolAST "print", x]) jmp =
    let (xAST, xBytecode, jmp_1) = astToBytecode' (AST [x]) jmp
    in (xAST, xBytecode ++ [Call 1], jmp_1)
astToBytecode' (AST [SymbolAST "exit", x]) jmp =
    let (xAST, xBytecode, jmp_1) = astToBytecode' (AST [x]) jmp
    in (xAST, xBytecode ++ [Call 60], jmp_1)


astToBytecode' (AST [IntTypeAST, SymbolAST x]) jmp =    (AST [], [LoadVarBefore x IntType], jmp)
astToBytecode' (AST [CharTypeAST, SymbolAST x]) jmp =   (AST [], [LoadVarBefore x CharType], jmp)
astToBytecode' (AST [StringTypeAST, SymbolAST x]) jmp = (AST [], [LoadVarBefore x StringType], jmp)
astToBytecode' (AST [FloatTypeAST, SymbolAST x]) jmp =  (AST [], [LoadVarBefore x FloatType], jmp)

astToBytecode' (AST (SymbolAST x : y : xs)) jmp = do
    let (_, aBytecode, _) = astToBytecode' y jmp
    let (yAST, yBytecode, jmp_2) = astToBytecode' (AST xs) jmp
    (yAST, (aBytecode ++ [LoadPC, CallUserFun x] ++ yBytecode), jmp_2)

-- * (AST (x:xs))
astToBytecode' (AST (x:xs)) jmp =
    let (_, xBytecode, jmp_1) = astToBytecode' x jmp
        (xsAST, xsBytecode, jmp_2) = astToBytecode' (AST xs) jmp_1
    in (xsAST, xBytecode ++ xsBytecode, jmp_2)

-- FunTypeAST return_type
astToBytecode' (FunAST name args (FunTypeAST return_type) scope) jmp = do
    let (_, scopeBytecode, jmp_2) = astToBytecode' scope jmp
    (AST [], [FunEntryPoint name (getTypes return_type)] ++ (astStoreArgs args) ++ scopeBytecode ++ [Return], jmp_2)

-- * IF / ELSE IF / ELSE
astToBytecode' (IfAST cond expr1 elseIfExpr1) jmp = do
    let condBytecode = astConditionToBytecode cond
    let (_, expr1Bytecode, jmp1) = astToBytecode' expr1 jmp
    let (_, elseIfExpr1Bytecode, jmp2) = astToBytecode' elseIfExpr1 jmp1
    let new_jmp = (jmp + (jmp1 - jmp) + (jmp2 - jmp) + 1)
    if elseIfExpr1 == DeadLeafAST then
        (AST [], condBytecode ++ [JumpIfFalseBefore new_jmp] ++ expr1Bytecode ++ [JumpRef new_jmp] ++ elseIfExpr1Bytecode, new_jmp)
    else
        (AST [], condBytecode ++ [JumpIfFalseBefore new_jmp] ++ expr1Bytecode ++ [JumpBefore (new_jmp + 1)] ++ [JumpRef new_jmp] ++ elseIfExpr1Bytecode ++ [JumpRef (new_jmp + 1)], new_jmp + 1)

astToBytecode' (ElseIfAST cond expr1 elseIfExpr1) jmp = do
    let condBytecode = astConditionToBytecode cond
    let (_, expr1Bytecode, jmp1) = astToBytecode' expr1 jmp
    let (_, elseIfExpr1Bytecode, jmp2) = astToBytecode' elseIfExpr1 jmp1
    let new_jmp = (jmp + (jmp1 - jmp) + (jmp2 - jmp) + 1)
    if elseIfExpr1 == DeadLeafAST then
        (AST [], condBytecode ++ [JumpIfFalseBefore new_jmp] ++ expr1Bytecode ++ [JumpRef new_jmp] ++ elseIfExpr1Bytecode, new_jmp)
    else
        (AST [], condBytecode ++ [JumpIfFalseBefore new_jmp] ++ expr1Bytecode ++ [JumpBefore (new_jmp + 1)] ++ [JumpRef new_jmp] ++ elseIfExpr1Bytecode ++ [JumpRef (new_jmp + 1)], new_jmp + 1)

astToBytecode' (ElseAST expr1) jmp = do
    let (_, expr1Bytecode, jmp_1) = astToBytecode' expr1 jmp
    (AST [], expr1Bytecode, jmp_1)

-- * WHILE
astToBytecode' (WhileAST cond expr1) jmp = do
    let condBytecode = astConditionToBytecode cond
    let (_, expr1Bytecode, jmp1) = astToBytecode' expr1 jmp
    let new_jmp = (jmp + (jmp1 - jmp) + 1)
    (AST [], [JumpRef (new_jmp + 1)] ++ condBytecode ++ [JumpIfFalseBefore new_jmp] ++ expr1Bytecode ++ [JumpBefore (new_jmp + 1)] ++ [JumpRef new_jmp], new_jmp + 1)

-- * FOR
astToBytecode' (ForAST initi cond increment scope) jmp = do
    let (_, initiBytecode, jmp1) = astToBytecode' initi jmp
    let condBytecode = astConditionToBytecode cond
    let (_, incrementBytecode, jmp2) = astToBytecode' increment jmp1
    let (_, scopeBytecode, jmp3) = astToBytecode' scope jmp2
    let new_jmp = jmp + (jmp1 - jmp) + (jmp2 - jmp1) + (jmp3 - jmp2) + 1
    (AST [], initiBytecode ++ [JumpRef (new_jmp + 1)] ++ condBytecode ++ [JumpIfFalseBefore new_jmp] ++ scopeBytecode ++ incrementBytecode ++ [JumpBefore (new_jmp + 1)] ++ [JumpRef new_jmp], new_jmp + 1)

-- * RETURN
astToBytecode' (ReturnAST expr1) jmp =
    let (_, expr1Bytecode, jmp_1) = astToBytecode' expr1 jmp
    in (AST [], expr1Bytecode ++ [Return], jmp_1)

-- * Assignation operation
astToBytecode' (AssignAST x y) jmp =
    let (yAST, yBytecode, jmp_1) = astToBytecode' y jmp
    in (yAST, yBytecode ++ (astStoreValue x), jmp_1)

-- * Simple operations
astToBytecode' (PlusAST x y) jmp =
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) jmp
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) jmp1
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "+"]], jmp2)

astToBytecode' (MinusAST x y) jmp =
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) jmp
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) jmp1
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "-"]], jmp2)

astToBytecode' (TimesAST x y) jmp =
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) jmp
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) jmp1
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "*"]], jmp2)

astToBytecode' (DivideAST x y) jmp =
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) jmp
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) jmp1
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "/"]], jmp2)

astToBytecode' (ModuloAST x y) jmp =
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) jmp
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) jmp1
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "%"]], jmp2)

-- * Incrementation and decrementation
astToBytecode' (IncrementAST x) jmp =
    let (_, xBytecode, jmp1) = astToBytecode' x jmp
        incrementCode = [LoadConst 1 IntType, BinaryOp "+"]
        storeCode = astStoreValue x
    in (AST [], xBytecode ++ incrementCode ++ storeCode, jmp1)

astToBytecode' (DecrementAST x) jmp =
    let (_, xBytecode, jmp1) = astToBytecode' x jmp
        decrementCode = [LoadConst 1 IntType, BinaryOp "-"]
        storeCode = astStoreValue x
    in (AST [], xBytecode ++ decrementCode ++ storeCode, jmp1)

-- * Assignation avec opération (ex: +=, -=, /=, %=)
astToBytecode' (PlusEqualAST x y) jmp =
    let (_, xBytecode, jmp1) = astToBytecode' x jmp
        (_, yBytecode, jmp2) = astToBytecode' y jmp1
        storeCode = astStoreValue x
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "+"] ++ storeCode, jmp2)

astToBytecode' (MinusEqualAST x y) jmp =
    let (_, xBytecode, jmp1) = astToBytecode' x jmp
        (_, yBytecode, jmp2) = astToBytecode' y jmp1
        storeCode = astStoreValue x
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "-"] ++ storeCode, jmp2)

astToBytecode' (TimesEqualAST x y) jmp =
    let (_, xBytecode, jmp1) = astToBytecode' x jmp
        (_, yBytecode, jmp2) = astToBytecode' y jmp1
        storeCode = astStoreValue x
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "*"] ++ storeCode, jmp2)

astToBytecode' (DivideEqualAST x y) jmp =
    let (_, xBytecode, jmp1) = astToBytecode' x jmp
        (_, yBytecode, jmp2) = astToBytecode' y jmp1
        storeCode = astStoreValue x
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "/"] ++ storeCode, jmp2)

-- * && and ||
astToBytecode' (AndAST x y) jmp =
    let (_, xBytecode, jmp1) = astToBytecode' x jmp
        (_, yBytecode, jmp2) = astToBytecode' y jmp1
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "&&"], jmp2)

astToBytecode' (OrAST x y) jmp =
    let (_, xBytecode, jmp1) = astToBytecode' x jmp
        (_, yBytecode, jmp2) = astToBytecode' y jmp1
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "||"], jmp2)

-- * Load operations
astToBytecode' (SymbolAST x) jmp =   (AST [], [LoadVarBefore x UnknownType], jmp) -- we can't know the type of the variable
astToBytecode' (IntAST x) jmp =      (AST [], [LoadConst x IntType], jmp)
astToBytecode' (FloatAST x) jmp =    (AST [], [LoadConst (floatToInt x) FloatType], jmp)
astToBytecode' (CharAST x) jmp =     (AST [], [LoadConst (fromEnum x) CharType], jmp)
astToBytecode' (StringAST x) jmp =   (AST [], [LoadConst 0 StringType, StringToSave x], jmp)
astToBytecode' DeadLeafAST jmp =     (AST [], [], jmp)
astToBytecode' a jmp = (a, [], jmp)

floatToInt :: Float -> Int
floatToInt = unsafeCoerce
