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


astConditionToBytecode :: AST -> [Bytecode] -> [Bytecode]
astConditionToBytecode (AST []) bytecode = bytecode
astConditionToBytecode (EqualAST            cond1 cond2) bytecode = bytecode ++ (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp "=="]
astConditionToBytecode (LessThanAST         cond1 cond2) bytecode = bytecode ++ (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp "<"]
astConditionToBytecode (GreaterThanAST      cond1 cond2) bytecode = bytecode ++ (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp ">"]
astConditionToBytecode (LessThanEqualAST    cond1 cond2) bytecode = bytecode ++ (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp "<="]
astConditionToBytecode (GreaterThanEqualAST cond1 cond2) bytecode = bytecode ++ (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp ">="]
astConditionToBytecode (NotEqualAST         cond1 cond2) bytecode = bytecode ++ (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp "!="]
-- astConditionToBytecode (AndAST cond1 cond2) bytecode =
-- astConditionToBytecode (OrAST cond1 cond2) bytecode =
astConditionToBytecode x bytecode = trace ("astConditionToBytecode NO AST CONDITION NODE FOUND: " ++ show x) bytecode


astStoreValue :: AST -> [Bytecode]
astStoreValue (AST [IntTypeAST, SymbolAST x]) = trace ("Get Value Int symbol " ++ show x) $ [StoreVar x]
astStoreValue (AST [SymbolAST x]) = trace ("Get Value Symbol " ++ show x) $ [StoreVar x]
astStoreValue _ = trace ("astStoreValue NO AST STORE NODE FOUND") $ []

astStoreArgs :: AST -> [Bytecode]
astStoreArgs DeadLeafAST = trace ("astStoreArgs empty") $ []
astStoreArgs (AST []) = trace ("astStoreArgs End") $ []
astStoreArgs (AST (x:xs)) = astStoreValue x ++ astStoreArgs (AST xs)
astStoreArgs x = trace ("astStoreArgs ERROR " ++ show x) $ []


-- TODO the argument  [Bytecode] is useless, remove it

-- INFO: This function takes an AST and returns a list of Bytecode instructions
--       that can be executed by the VM.

--             AST      bytecode  id_jmp -> AST bytecode
astToBytecode' :: AST -> [Bytecode] -> Int -> (AST, [Bytecode], Int)
astToBytecode' (AST []) bytecode jmp = (AST [], bytecode, jmp)

-- * System calls
astToBytecode' (AST [SymbolAST "print", x]) bytecode jmp =
    let (xAST, xBytecode, jmp') = astToBytecode' (AST [x]) bytecode jmp
    in (xAST, xBytecode ++ [Call 1], jmp')
astToBytecode' (AST [SymbolAST "exit", x]) bytecode jmp =
    let (xAST, xBytecode, jmp') = astToBytecode' (AST [x]) bytecode jmp
    in (xAST, xBytecode ++ [Call 60], jmp')

-- how say to eval part to put all the arg variable equal to the id of the id local
-- call(c, d)
-- {
--     here c = a and d = b
-- }
-- main()
-- {
--     a,b,
--     call(a,b)
-- }

astToBytecode' (AST (SymbolAST x : (AST y) : xs)) bytecode jmp = do
    let (yAST, yBytecode, jmp_2) = astToBytecode' (AST xs) bytecode jmp
    trace ("call function " ++ show x ++ " args " ++ show y) $ (yAST, bytecode ++ (astStoreArgs (AST y) ++ [LoadPC, CallUserFunBefore x] ++ yBytecode), jmp_2)

-- * (AST (x:xs))
astToBytecode' (AST (x:xs)) bytecode jmp = trace ("Processing AST node: " ++ show x) $
    let (_, xBytecode, jmp') = astToBytecode' x bytecode jmp
        (xsAST, xsBytecode, jmp'') = astToBytecode' (AST xs) bytecode jmp'
    in (xsAST, xBytecode ++ xsBytecode, jmp'')

-- FunTypeAST return_type
astToBytecode' (FunAST name args return_type scope) bytecode jmp = trace ("FunAST: " ++ show name ++ " " ++ show args ++ " " ++ show return_type ++ " " ++ show scope) $ do
    let (_, scopeBytecode, jmp_2) = trace ("scopeAST: " ++ show scope) astToBytecode' scope bytecode jmp
    trace ("scopeBytecode " ++ show scopeBytecode) (AST [], bytecode ++ [FunEntryPointBefore name] ++ (astStoreArgs args) ++ scopeBytecode ++ [Return], jmp_2)
    -- trace ("scopeBytecode " ++ show scopeBytecode) (AST [], bytecode ++ [FunEntryPointBefore name] ++ [PushFrame] ++ (astStoreArgs args) ++ scopeBytecode ++ [PopFrame, Return], jmp_2) -- TODO add return type

-- * IF / ELSE IF / ELSE
astToBytecode' (IfAST cond expr1 elseIfExpr1) bytecode jmp = trace ("IfAST: " ++ show cond ++ " |expr1| " ++ show expr1 ++ " |do| " ++ show elseIfExpr1) $ do
    let condBytecode = trace ("condBytecode1: " ++ show cond) astConditionToBytecode cond bytecode
    let (_, expr1Bytecode, jmp1) = trace ("expr1AST: " ++ show expr1) astToBytecode' expr1 bytecode jmp
    let (_, elseIfExpr1Bytecode, jmp2) = trace ("elseIfExpr1: " ++ show elseIfExpr1 ++ "\n\n") astToBytecode' elseIfExpr1 bytecode jmp1
    let new_jmp = trace ("new_jmp  " ++ show (jmp + (jmp1 - jmp) + (jmp2 - jmp) + 1)) (jmp + (jmp1 - jmp) + (jmp2 - jmp) + 1)
    if elseIfExpr1 == DeadLeafAST then
        (AST [], bytecode ++ condBytecode ++ [JumpIfFalseBefore new_jmp] ++ expr1Bytecode ++ [JumpRef new_jmp] ++ elseIfExpr1Bytecode, new_jmp)
    else
        (AST [], bytecode ++ condBytecode ++ [JumpIfFalseBefore new_jmp] ++ expr1Bytecode ++ [JumpBefore (new_jmp + 1)] ++ [JumpRef new_jmp] ++ elseIfExpr1Bytecode ++ [JumpRef (new_jmp + 1)], new_jmp + 1)

astToBytecode' (ElseIfAST cond expr1 elseIfExpr1) bytecode jmp = trace ("ElseIfAST: " ++ show cond ++ " |expr1| " ++ show expr1 ++ " |do| " ++ show elseIfExpr1) $ do
    let condBytecode = trace ("condBytecode1: " ++ show cond) astConditionToBytecode cond bytecode
    let (_, expr1Bytecode, jmp1) = trace ("expr1AST: " ++ show expr1) astToBytecode' expr1 bytecode jmp
    let (_, elseIfExpr1Bytecode, jmp2) = trace ("elseIfExpr1: " ++ show elseIfExpr1 ++ "\n\n") astToBytecode' elseIfExpr1 bytecode jmp1
    let new_jmp = trace ("new_jmp  " ++ show (jmp + (jmp1 - jmp) + (jmp2 - jmp) + 1)) (jmp + (jmp1 - jmp) + (jmp2 - jmp) + 1)
    if elseIfExpr1 == DeadLeafAST then
        (AST [], bytecode ++ condBytecode ++ [JumpIfFalseBefore new_jmp] ++ expr1Bytecode ++ [JumpRef new_jmp] ++ elseIfExpr1Bytecode, new_jmp)
    else
        (AST [], bytecode ++ condBytecode ++ [JumpIfFalseBefore new_jmp] ++ expr1Bytecode ++ [JumpBefore (new_jmp + 1)] ++ [JumpRef new_jmp] ++ elseIfExpr1Bytecode ++ [JumpRef (new_jmp + 1)], new_jmp + 1)

astToBytecode' (ElseAST expr1) bytecode jmp = trace ("ElseAST: " ++ show expr1) $ do
    let (_, expr1Bytecode, jmp') = trace ("expr1AST: " ++ show expr1) astToBytecode' expr1 bytecode jmp
    (AST [], bytecode ++ expr1Bytecode, jmp')

-- * WHILE
astToBytecode' (WhileAST cond expr1) bytecode jmp = trace ("WhileAST: " ++ show cond ++ " |expr1| " ++ show expr1) $ do
    let condBytecode = trace ("condBytecode1: " ++ show cond) astConditionToBytecode cond bytecode
    let (_, expr1Bytecode, jmp1) = trace ("expr1AST: " ++ show expr1) astToBytecode' expr1 bytecode jmp
    let new_jmp = trace ("new_jmp  " ++ show (jmp + (jmp1 - jmp) + 1)) (jmp + (jmp1 - jmp) + 1)
    (AST [], bytecode ++ [JumpRef (new_jmp + 1)] ++ condBytecode ++ [JumpIfFalseBefore new_jmp] ++ expr1Bytecode ++ [JumpBefore (new_jmp + 1)] ++ [JumpRef new_jmp], new_jmp + 1)

-- * FOR
astToBytecode' (ForAST initi cond increment scope) bytecode jmp = do
    let (_, initiBytecode, jmp1) = astToBytecode' initi bytecode jmp
    let condBytecode = astConditionToBytecode cond bytecode
    let (_, incrementBytecode, jmp2) = astToBytecode' increment bytecode jmp1
    let (_, scopeBytecode, jmp3) = astToBytecode' scope bytecode jmp2
    let new_jmp = jmp + (jmp1 - jmp) + (jmp2 - jmp1) + (jmp3 - jmp2) + 1
    (AST [], bytecode ++ initiBytecode ++ [JumpRef (new_jmp + 1)] ++ condBytecode ++ [JumpIfFalseBefore new_jmp] ++ scopeBytecode ++ incrementBytecode ++ [JumpBefore (new_jmp + 1)] ++ [JumpRef new_jmp], new_jmp + 1)

-- * RETURN
astToBytecode' (ReturnAST (AST expr1)) bytecode jmp =
    let (_, expr1Bytecode, jmp') = trace ("ReturnAST: " ++ show expr1) astToBytecode' (AST expr1) bytecode jmp
    in (AST [], bytecode ++ expr1Bytecode ++ [Return], jmp')

-- * Assignation operation
astToBytecode' (AssignAST x y) bytecode jmp = trace ("AssignAST: " ++ show x ++ " = " ++ show y) $
    let (yAST, yBytecode, jmp') = astToBytecode' y bytecode jmp
    in (yAST, yBytecode ++ (astStoreValue x), jmp')

-- * Simple operations
astToBytecode' (PlusAST x y) bytecode jmp = trace ("PlusAST: " ++ show x ++ " + " ++ show y) $
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) bytecode jmp
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) bytecode jmp1
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "+"]], jmp2)

astToBytecode' (MinusAST x y) bytecode jmp = trace ("MinusAST: " ++ show x ++ " - " ++ show y) $
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) bytecode jmp
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) bytecode jmp1
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "-"]], jmp2)

astToBytecode' (TimesAST x y) bytecode jmp = trace ("TimesAST: " ++ show x ++ " * " ++ show y) $
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) bytecode jmp
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) bytecode jmp1
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "*"]], jmp2)

astToBytecode' (DivideAST x y) bytecode jmp = trace ("DivideAST: " ++ show x ++ " / " ++ show y) $
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) bytecode jmp
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) bytecode jmp1
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "/"]], jmp2)

astToBytecode' (ModuloAST x y) bytecode jmp = trace ("ModuloAST: " ++ show x ++ " % " ++ show y) $
    let (_, xBytecode, jmp1) = astToBytecode' (AST [x]) bytecode jmp
        (_, yBytecode, jmp2) = astToBytecode' (AST [y]) bytecode jmp1
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "%"]], jmp2)

-- * Load operations
astToBytecode' (SymbolAST x) bytecode jmp = trace ("SymbolAST: " ++ show x) $ astToBytecode' (AST []) (bytecode ++ [LoadVar x]) jmp
astToBytecode' (IntAST x) bytecode jmp = trace ("IntAST: " ++ show x) $ astToBytecode' (AST []) (bytecode ++ [LoadConst x]) jmp
astToBytecode' DeadLeafAST bytecode jmp = trace ("DeadLeafAST") $ astToBytecode' (AST []) bytecode jmp
astToBytecode' a b jmp = trace ("Unknown AST node bytecode: " ++ show a ++ " " ++ show b) (a, b, jmp)
