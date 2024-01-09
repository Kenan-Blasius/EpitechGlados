module AstToBytecode (
    astToBytecode',
    astConditionToBytecode,
    valueSimpleToBytecode,
    astStoreValue,
) where

import Types
import Debug.Trace

-- | IfAST AST AST AST
-- | ElseIfAST AST AST AST
-- | ElseAST AST
-- | DefineAST String AST

-- | ForAST AST AST AST AST
-- | WhileAST AST AST

-- | FunAST String AST AST AST
-- | LambdaClosure [String] AST Environment

-- | AssignAST AST AST
-- | FunTypeAST AST
-- | IntTypeAST
-- | CharTypeAST
-- | StringTypeAST

-- | IntAST Int
-- | SymbolAST String
-- | StringAST String
-- | CharAST Char

-- | EqualAST AST AST
-- | LessThanAST AST AST
-- | GreaterThanAST AST AST
-- | LessThanEqualAST AST AST
-- | GreaterThanEqualAST AST AST
-- | NotEqualAST AST AST

-- | PlusAST AST AST
-- | MinusAST AST AST
-- | TimesAST AST AST
-- | DivideAST AST AST
-- | ModuloAST AST AST

-- | AndAST AST AST
-- | OrAST AST AST

-- | PlusEqualAST AST AST
-- | MinusEqualAST AST AST
-- | TimesEqualAST AST AST
-- | DivideEqualAST AST AST
-- | ModuloEqualAST AST AST
-- | NotAST AST
-- | IncrementAST AST
-- | DecrementAST AST
-- | DeadLeafAST



-- TODO && ||
-- TODO += -= *= /= %=
-- TODO ++ --
-- TODO !

valueSimpleToBytecode :: AST -> [Bytecode]
valueSimpleToBytecode (AST []) = []
valueSimpleToBytecode (AST (x:_)) = trace ("valueSimpleToBytecode AST (x:xs): " ++ show x) $
    case x of
        IntAST y -> trace ("valueSimpleToBytecode IntAST: " ++ show y) [LoadConst y]
        SymbolAST y -> trace ("valueSimpleToBytecode SymbolAST: " ++ show y) [LoadVar y]
        y -> trace ("valueSimpleToBytecode NO AST SIMPLE NODE FOUND: " ++ show y) []
valueSimpleToBytecode x = trace ("valueSimpleToBytecode NO AST SIMPLE NODE FOUND: " ++ show x) []


astConditionToBytecode :: AST -> [Bytecode]
astConditionToBytecode (AST []) = []
astConditionToBytecode (EqualAST            cond1 cond2) = (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp "=="]
astConditionToBytecode (LessThanAST         cond1 cond2) = (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp "<"]
astConditionToBytecode (GreaterThanAST      cond1 cond2) = (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp ">"]
astConditionToBytecode (LessThanEqualAST    cond1 cond2) = (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp "<="]
astConditionToBytecode (GreaterThanEqualAST cond1 cond2) = (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp ">="]
astConditionToBytecode (NotEqualAST         cond1 cond2) = (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp "!="]
-- astConditionToBytecode (AndAST cond1 cond2) =
-- astConditionToBytecode (OrAST cond1 cond2) =
astConditionToBytecode x = trace ("astConditionToBytecode NO AST CONDITION NODE FOUND: " ++ show x) []


astStoreValue :: AST -> [Bytecode]
astStoreValue (AST [IntTypeAST, SymbolAST x]) = trace ("Get Value Int symbol " ++ show x) $ [StoreVar x]
astStoreValue (AST [SymbolAST x]) = trace ("Get Value Symbol " ++ show x) $ [StoreVar x]
astStoreValue x = trace ("astStoreValue NO AST STORE NODE FOUND" ++ show x) $ []

astStoreArgs :: AST -> [Bytecode]
astStoreArgs DeadLeafAST = trace ("astStoreArgs empty") $ []
astStoreArgs (AST []) = trace ("astStoreArgs End") $ []
astStoreArgs (AST ((AST x) :xs)) = trace ("astStoreArgs AST " ++ show x) $ astStoreArgs (AST x) ++ astStoreArgs (AST xs)
astStoreArgs x = astStoreValue x

-- maybe usefull for scary function
-- pushArgs :: AST -> [Bytecode]
-- pushArgs DeadLeafAST = trace ("pushArgs empty") $ []
-- pushArgs (AST []) = trace ("pushArgs End") $ []
-- pushArgs (AST [IntTypeAST, SymbolAST x]) = trace ("pushArgs Int symbol " ++ show x) $ [LoadVar x]
-- pushArgs (AST [SymbolAST x]) = trace ("pushArgs Symbol " ++ show x) $ [LoadVar x]
-- pushArgs (AST (x:xs)) = pushArgs x ++ pushArgs (AST xs)
-- pushArgs x = trace ("pushArgs ERROR " ++ show x) $ []

-- INFO: This function takes an AST and returns a list of Bytecode instructions
--       that can be executed by the VM.

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

-- maybe check the type of SymbolAST (variable, function, string...)
-- ! scary but we keep it for now
astToBytecode' (AST (SymbolAST x : y : xs)) jmp = do
    let (_, aBytecode, _) = astToBytecode' y jmp
    let (yAST, yBytecode, jmp_2) = trace ("call function " ++ show x ++ " args " ++ show y ++ " xs " ++ show xs) $ astToBytecode' (AST xs) jmp
    trace ("call function " ++ show x ++ " args " ++ show y) $ (yAST, (aBytecode ++ [LoadPC, CallUserFun x] ++ yBytecode), jmp_2)

-- ! old version
-- astToBytecode' (AST (SymbolAST x : (AST y) : xs)) bytecode jmp = do
--     let (yAST, yBytecode, jmp_2) = astToBytecode' (AST xs) bytecode jmp
--     trace ("call function " ++ show x ++ " args " ++ show y) $ (yAST, bytecode ++ (pushArgs (AST y) ++ [LoadPC, CallUserFun x (nmbArgs (AST y))] ++ yBytecode), jmp_2)

-- * (AST (x:xs))
astToBytecode' (AST (x:xs)) jmp = trace ("Processing AST node: " ++ show x) $
    let (_, xBytecode, jmp_1) = astToBytecode' x jmp
        (xsAST, xsBytecode, jmp_2) = astToBytecode' (AST xs) jmp_1
    in (xsAST, xBytecode ++ xsBytecode, jmp_2)

-- FunTypeAST return_type
astToBytecode' (FunAST name args return_type scope) jmp = trace ("FunAST: " ++ show name ++ " " ++ show args ++ " " ++ show return_type ++ " " ++ show scope) $ do
    let (_, scopeBytecode, jmp_2) = trace ("scopeAST: " ++ show scope) astToBytecode' scope jmp
    trace ("scopeBytecode " ++ show scopeBytecode) (AST [], [FunEntryPoint name] ++ (astStoreArgs args) ++ scopeBytecode ++ [Return], jmp_2)
    -- trace ("scopeBytecode " ++ show scopeBytecode) (AST [], [FunEntryPoint name] ++ [PushFrame] ++ (astStoreArgs args) ++ scopeBytecode ++ [PopFrame, Return], jmp_2) -- TODO add return type

-- * IF / ELSE IF / ELSE
astToBytecode' (IfAST cond expr1 elseIfExpr1) jmp = trace ("IfAST: " ++ show cond ++ " |expr1| " ++ show expr1 ++ " |do| " ++ show elseIfExpr1) $ do
    let condBytecode = trace ("condBytecode1: " ++ show cond) (astConditionToBytecode cond)
    let (_, expr1Bytecode, jmp1) = trace ("expr1AST: " ++ show expr1) (astToBytecode' expr1 jmp)
    let (_, elseIfExpr1Bytecode, jmp2) = trace ("elseIfExpr1: " ++ show elseIfExpr1 ++ "\n\n") (astToBytecode' elseIfExpr1 jmp1)
    let new_jmp = trace ("new_jmp  " ++ show (jmp + (jmp1 - jmp) + (jmp2 - jmp) + 1)) (jmp + (jmp1 - jmp) + (jmp2 - jmp) + 1)
    if elseIfExpr1 == DeadLeafAST then
        (AST [], condBytecode ++ [JumpIfFalseBefore new_jmp] ++ expr1Bytecode ++ [JumpRef new_jmp] ++ elseIfExpr1Bytecode, new_jmp)
    else
        (AST [], condBytecode ++ [JumpIfFalseBefore new_jmp] ++ expr1Bytecode ++ [JumpBefore (new_jmp + 1)] ++ [JumpRef new_jmp] ++ elseIfExpr1Bytecode ++ [JumpRef (new_jmp + 1)], new_jmp + 1)

astToBytecode' (ElseIfAST cond expr1 elseIfExpr1) jmp = trace ("ElseIfAST: " ++ show cond ++ " |expr1| " ++ show expr1 ++ " |do| " ++ show elseIfExpr1) $ do
    let condBytecode = trace ("condBytecode1: " ++ show cond) (astConditionToBytecode cond)
    let (_, expr1Bytecode, jmp1) = trace ("expr1AST: " ++ show expr1) (astToBytecode' expr1 jmp)
    let (_, elseIfExpr1Bytecode, jmp2) = trace ("elseIfExpr1: " ++ show elseIfExpr1 ++ "\n\n") (astToBytecode' elseIfExpr1 jmp1)
    let new_jmp = trace ("new_jmp  " ++ show (jmp + (jmp1 - jmp) + (jmp2 - jmp) + 1)) (jmp + (jmp1 - jmp) + (jmp2 - jmp) + 1)
    if elseIfExpr1 == DeadLeafAST then
        (AST [], condBytecode ++ [JumpIfFalseBefore new_jmp] ++ expr1Bytecode ++ [JumpRef new_jmp] ++ elseIfExpr1Bytecode, new_jmp)
    else
        (AST [], condBytecode ++ [JumpIfFalseBefore new_jmp] ++ expr1Bytecode ++ [JumpBefore (new_jmp + 1)] ++ [JumpRef new_jmp] ++ elseIfExpr1Bytecode ++ [JumpRef (new_jmp + 1)], new_jmp + 1)

astToBytecode' (ElseAST expr1) jmp = trace ("ElseAST: " ++ show expr1) $ do
    let (_, expr1Bytecode, jmp_1) = trace ("expr1AST: " ++ show expr1) (astToBytecode' expr1 jmp)
    (AST [], expr1Bytecode, jmp_1)

-- * WHILE
astToBytecode' (WhileAST cond expr1) jmp = trace ("WhileAST: " ++ show cond ++ " |expr1| " ++ show expr1) $ do
    let condBytecode = trace ("condBytecode1: " ++ show cond) (astConditionToBytecode cond)
    let (_, expr1Bytecode, jmp1) = trace ("expr1AST: " ++ show expr1) (astToBytecode' expr1 jmp)
    let new_jmp = trace ("new_jmp  " ++ show (jmp + (jmp1 - jmp) + 1)) (jmp + (jmp1 - jmp) + 1)
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
astToBytecode' (ReturnAST (AST expr1)) jmp =
    let (_, expr1Bytecode, jmp_1) = trace ("ReturnAST: " ++ show expr1) (astToBytecode' (AST expr1) jmp)
    in (AST [], expr1Bytecode ++ [Return], jmp_1)

-- * Assignation operation
astToBytecode' (AssignAST x y) jmp = trace ("AssignAST: " ++ show x ++ " = " ++ show y) $
    let (yAST, yBytecode, jmp_1) = astToBytecode' y jmp
    in (yAST, yBytecode ++ (astStoreValue x), jmp_1)

-- * Simple operations
astToBytecode' (PlusAST x y) jmp = trace ("PlusAST: " ++ show x ++ " + " ++ show y) $
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) jmp
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) jmp1
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "+"]], jmp2)

astToBytecode' (MinusAST x y) jmp = trace ("MinusAST: " ++ show x ++ " - " ++ show y) $
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) jmp
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) jmp1
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "-"]], jmp2)

astToBytecode' (TimesAST x y) jmp = trace ("TimesAST: " ++ show x ++ " * " ++ show y) $
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) jmp
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) jmp1
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "*"]], jmp2)

astToBytecode' (DivideAST x y) jmp = trace ("DivideAST: " ++ show x ++ " / " ++ show y) $
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) jmp
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) jmp1
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "/"]], jmp2)

astToBytecode' (ModuloAST x y) jmp = trace ("ModuloAST: " ++ show x ++ " % " ++ show y) $
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) jmp
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) jmp1
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "%"]], jmp2)

-- * Incrementation and decrementation
astToBytecode' (IncrementAST x) jmp = trace ("IncrementAST: " ++ show x) $
    let (_, xBytecode, jmp1) = astToBytecode' x jmp
        incrementCode = [LoadConst 1, BinaryOp "+"]
        storeCode = astStoreValue x
    in (AST [], xBytecode ++ incrementCode ++ storeCode, jmp1)

astToBytecode' (DecrementAST x) jmp = trace ("DecrementAST: " ++ show x) $
    let (_, xBytecode, jmp1) = astToBytecode' x jmp
        decrementCode = [LoadConst 1, BinaryOp "-"]
        storeCode = astStoreValue x
    in (AST [], xBytecode ++ decrementCode ++ storeCode, jmp1)

-- * Assignation avec opération (ex: +=, -=, /=, %=)
astToBytecode' (PlusEqualAST x y) jmp = trace ("PlusEqualAST: " ++ show x ++ " += " ++ show y) $
    let (_, xBytecode, jmp1) = astToBytecode' x jmp
        (_, yBytecode, jmp2) = astToBytecode' y jmp1
        storeCode = astStoreValue x
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "+"] ++ storeCode, jmp2)

astToBytecode' (MinusEqualAST x y) jmp = trace ("MinusEqualAST: " ++ show x ++ " -= " ++ show y) $
    let (_, xBytecode, jmp1) = astToBytecode' x jmp
        (_, yBytecode, jmp2) = astToBytecode' y jmp1
        storeCode = astStoreValue x
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "-"] ++ storeCode, jmp2)

astToBytecode' (TimesEqualAST x y) jmp = trace ("TimesEqualAST: " ++ show x ++ " *= " ++ show y) $
    let (_, xBytecode, jmp1) = astToBytecode' x jmp
        (_, yBytecode, jmp2) = astToBytecode' y jmp1
        storeCode = astStoreValue x
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "*"] ++ storeCode, jmp2)

astToBytecode' (DivideEqualAST x y) jmp = trace ("DivideEqualAST: " ++ show x ++ " /= " ++ show y) $
    let (_, xBytecode, jmp1) = astToBytecode' x jmp
        (_, yBytecode, jmp2) = astToBytecode' y jmp1
        storeCode = astStoreValue x
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "/"] ++ storeCode, jmp2)

-- * && and ||
astToBytecode' (AndAST x y) jmp = trace ("AndAST: " ++ show x ++ " && " ++ show y) $
    let (_, xBytecode, jmp1) = astToBytecode' x jmp
        (_, yBytecode, jmp2) = astToBytecode' y jmp1
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "&&"], jmp2)

astToBytecode' (OrAST x y) jmp = trace ("OrAST: " ++ show x ++ " || " ++ show y) $
    let (_, xBytecode, jmp1) = astToBytecode' x jmp
        (_, yBytecode, jmp2) = astToBytecode' y jmp1
    in (AST [], xBytecode ++ yBytecode ++ [BinaryOp "||"], jmp2)

-- * Load operations
astToBytecode' (SymbolAST x) jmp = trace ("SymbolAST: " ++ show x) $ (AST [], [LoadVar x], jmp)
astToBytecode' (IntAST x) jmp = trace ("IntAST: " ++ show x) $ (AST [], [LoadConst x], jmp)
astToBytecode' DeadLeafAST jmp = trace ("DeadLeafAST") $ (AST [], [], jmp)
astToBytecode' a jmp = trace ("Unknown AST node bytecode: " ++ show a ++ " ") (a, [], jmp)
