module AstToBytecode (
    astToBytecode',
    astConditionToBytecode,
    valueSimpleToBytecode,
    astStoreValue,
    sizeInstructionOfAst
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




sizeInstructionOfAst :: AST -> Int -> Int
sizeInstructionOfAst (AST []) size = size
sizeInstructionOfAst (IntAST _) size = size + 1
sizeInstructionOfAst (AST (x:xs)) size = sizeInstructionOfAst (AST xs) (sizeInstructionOfAst x size)
sizeInstructionOfAst (SymbolAST _) size = size + 1
sizeInstructionOfAst (IfAST (AST cond) (AST expr1) (AST elseIfExpr1)) size =
    sizeInstructionOfAst (AST cond) $
    sizeInstructionOfAst (AST expr1) $
    sizeInstructionOfAst (AST elseIfExpr1) $
    size + 1
sizeInstructionOfAst (ElseAST expr1) size =
    sizeInstructionOfAst expr1 $
    size + 1
sizeInstructionOfAst (AssignAST x y) size =
    sizeInstructionOfAst x $
    sizeInstructionOfAst y $
    size + 1
sizeInstructionOfAst (PlusAST x y) size =
    sizeInstructionOfAst x $
    sizeInstructionOfAst y $
    size + 1
sizeInstructionOfAst (MinusAST x y) size =
    sizeInstructionOfAst x $
    sizeInstructionOfAst y $
    size + 1
sizeInstructionOfAst (TimesAST x y) size =
    sizeInstructionOfAst x $
    sizeInstructionOfAst y $
    size + 1
sizeInstructionOfAst (DivideAST x y) size =
    sizeInstructionOfAst x $
    sizeInstructionOfAst y $
    size + 1
sizeInstructionOfAst (ModuloAST x y) size =
    sizeInstructionOfAst x $
    sizeInstructionOfAst y $
    size + 1
sizeInstructionOfAst (EqualAST x y) size =
    sizeInstructionOfAst x $
    sizeInstructionOfAst y $
    size + 1
sizeInstructionOfAst (GreaterThanAST x y) size =
    sizeInstructionOfAst x $
    sizeInstructionOfAst y $
    size + 1
sizeInstructionOfAst (LessThanAST x y) size =
    sizeInstructionOfAst x $
    sizeInstructionOfAst y $
    size + 1
sizeInstructionOfAst (GreaterThanEqualAST x y) size =
    sizeInstructionOfAst x $
    sizeInstructionOfAst y $
    size + 1
sizeInstructionOfAst (LessThanEqualAST x y) size =
    sizeInstructionOfAst x $
    sizeInstructionOfAst y $
    size + 1
sizeInstructionOfAst (NotEqualAST x y) size =
    sizeInstructionOfAst x $
    sizeInstructionOfAst y $
    size + 1
sizeInstructionOfAst (WhileAST cond expr1) size =
    sizeInstructionOfAst cond $
    sizeInstructionOfAst expr1 $
    size + 1
sizeInstructionOfAst (ForAST initialisation cond expr1 expr2) size =
    sizeInstructionOfAst initialisation $
    sizeInstructionOfAst cond $
    sizeInstructionOfAst expr1 $
    sizeInstructionOfAst expr2 $
    size + 1
sizeInstructionOfAst (ReturnAST expr1) size =
    sizeInstructionOfAst expr1 $
    size + 1
sizeInstructionOfAst _ size = trace ("sizeInstructionOfAst No AST node found. Current size: " ++ show size) size




astConditionToBytecode :: AST -> [Bytecode] -> [Bytecode]
astConditionToBytecode (AST []) bytecode = bytecode
astConditionToBytecode (EqualAST cond1 cond2) bytecode = bytecode ++ (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp "=="]
astConditionToBytecode (LessThanAST cond1 cond2) bytecode = bytecode ++ (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp "<"]
astConditionToBytecode (GreaterThanAST cond1 cond2) bytecode = bytecode ++ (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp ">"]
astConditionToBytecode (LessThanEqualAST cond1 cond2) bytecode = bytecode ++ (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp "<="]
astConditionToBytecode (GreaterThanEqualAST cond1 cond2) bytecode = bytecode ++ (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp ">="]
astConditionToBytecode (NotEqualAST cond1 cond2) bytecode = bytecode ++ (valueSimpleToBytecode cond1) ++ (valueSimpleToBytecode cond2) ++ [CompareOp "!="]
astConditionToBytecode x bytecode = trace ("astConditionToBytecode NO AST CONDITION NODE FOUND: " ++ show x) bytecode

valueSimpleToBytecode :: AST -> [Bytecode]
valueSimpleToBytecode (AST []) = []
valueSimpleToBytecode (AST (x:_)) = trace ("valueSimpleToBytecode AST (x:xs): " ++ show x) $
    case x of
        IntAST y -> trace ("valueSimpleToBytecode IntAST: " ++ show y) [LoadConst y]
        SymbolAST y -> trace ("valueSimpleToBytecode SymbolAST: " ++ show y) [LoadVar y]
        y -> trace ("valueSimpleToBytecode NO AST SIMPLE NODE FOUND: " ++ show y) []
valueSimpleToBytecode x = trace ("valueSimpleToBytecode NO AST SIMPLE NODE FOUND: " ++ show x) []



astStoreValue :: AST -> [Bytecode] -> (AST, [Bytecode])
astStoreValue (AST [IntTypeAST, SymbolAST x]) bytecode = trace ("Get Value Int symbol " ++ show x) $ ((AST []), (bytecode ++ [StoreVar x]))
astStoreValue (AST [SymbolAST x]) bytecode = trace ("Get Value Symbol " ++ show x) $ ((AST []), (bytecode ++ [StoreVar x]))
astStoreValue _ _ = trace ("astStoreValue NO AST STORE NODE FOUND") $ ((AST []), [])



-- INFO: This function takes an AST and returns a list of Bytecode instructions
--       that can be executed by the VM.
astToBytecode' :: AST -> [Bytecode] -> (AST, [Bytecode])
astToBytecode' (AST []) bytecode = (AST [], bytecode)

-- ! only wait for AST ReturnAST
-- astToBytecode' (AST [SymbolAST "return", x]) bytecode =
--     let (xAST, xBytecode) = astToBytecode' (AST [x]) bytecode
--     in (xAST, xBytecode ++ [Return])
-- * System calls
astToBytecode' (AST [SymbolAST "print", x]) bytecode =
    let (xAST, xBytecode) = astToBytecode' (AST [x]) bytecode
    in (xAST, xBytecode ++ [Call 1]) -- system call 1 is write
astToBytecode' (AST [SymbolAST "exit", x]) bytecode =
    let (xAST, xBytecode) = astToBytecode' (AST [x]) bytecode
    in (xAST, xBytecode ++ [Call 60]) -- system call 60 is exit

-- * (AST (x:xs))
astToBytecode' (AST (x:xs)) bytecode = trace ("Processing AST node: " ++ show x) $
    let (_, xBytecode) = astToBytecode' x bytecode
        (xsAST, xsBytecode) = astToBytecode' (AST xs) bytecode
    in (xsAST, xBytecode ++ xsBytecode)

astToBytecode' (IfAST cond (AST expr1) (AST elseIfExpr1)) bytecode = trace ("IfAST: " ++ show cond ++ " |expr1| " ++ show expr1 ++ " |do| " ++ show elseIfExpr1) $ do
    let condBytecode = trace ("condBytecode1: " ++ show cond) astConditionToBytecode cond bytecode
    let (_, expr1Bytecode) = trace ("expr1AST: " ++ show expr1) astToBytecode' (AST expr1) bytecode
    let (_, elseIfExpr1Bytecode) = trace ("elseIfExpr1: " ++ show elseIfExpr1 ++ "\n\n") astToBytecode' (AST elseIfExpr1) bytecode
    let jmp_size = trace ("jmp_size: " ++ show expr1) (sizeInstructionOfAst (AST expr1) 0)
    (AST [], bytecode ++ condBytecode ++ [JumpIfFalse jmp_size] ++ expr1Bytecode ++ elseIfExpr1Bytecode)

-- ! check if good behaviour
astToBytecode' (ElseAST (AST expr1)) bytecode = trace ("ElseAST: " ++ show expr1) $ do
    let (_, expr1Bytecode) = trace ("expr1AST: " ++ show expr1) astToBytecode' (AST expr1) bytecode
    (AST [], bytecode ++ expr1Bytecode)

astToBytecode' (WhileAST cond (AST expr1)) bytecode = trace ("WhileAST: " ++ show cond ++ " |expr1| " ++ show expr1) $ do
    let condBytecode = trace ("condBytecode1: " ++ show cond) astConditionToBytecode cond bytecode
    let (_, expr1Bytecode) = trace ("expr1AST: " ++ show expr1) astToBytecode' (AST expr1) bytecode
    let jmp_size = trace ("jmp_size: " ++ show (sizeInstructionOfAst (AST expr1) 0)) (sizeInstructionOfAst (AST expr1) 0)
    let jmp_size2 = trace ("jmp_size2: " ++ show (sizeInstructionOfAst cond 0)) (sizeInstructionOfAst cond 0)
    (AST [], bytecode ++ condBytecode ++ [JumpIfFalse (jmp_size)] ++ expr1Bytecode ++ [Jump (-(jmp_size + jmp_size2 - 1))])

astToBytecode' (ReturnAST (AST expr1)) bytecode =
    let (_, expr1Bytecode) = trace ("ReturnAST: " ++ show expr1) astToBytecode' (AST expr1) bytecode
    in (AST [], bytecode ++ expr1Bytecode ++ [Return])

-- * to test lmao
-- astToBytecode' (ForAST (AST init) cond (AST expr1) (AST expr2)) bytecode = do
--     let (initAST, initBytecode) = trace ("initAST: " ++ show init) astToBytecode' (AST init) bytecode
--     let condBytecode = trace ("condBytecode1: " ++ show cond) astConditionToBytecode cond bytecode
--     let (expr1AST, expr1Bytecode) = trace ("expr1AST: " ++ show expr1) astToBytecode' (AST expr1) bytecode
--     let (expr2AST, expr2Bytecode) = trace ("expr2AST: " ++ show expr2) astToBytecode' (AST expr2) bytecode
--     (AST [], initBytecode ++ condBytecode ++ [JumpIfFalse (sizeInstructionOfAst (AST expr1) 0)] ++ expr1Bytecode ++ expr2Bytecode ++ [Jump (-(sizeInstructionOfAst (AST expr1) 0 + sizeInstructionOfAst cond 0 + sizeInstructionOfAst (AST expr2) 0))])

-- * Assignation operation
astToBytecode' (AssignAST x y) bytecode = trace ("AssignAST: " ++ show x ++ " = " ++ show y) $
    let (yAST, yBytecode) = astToBytecode' y bytecode
        (_, xBytecode) = astStoreValue x bytecode
    in (yAST, yBytecode ++ xBytecode)

-- * Simple operations
astToBytecode' (PlusAST x y) bytecode = trace ("PlusAST: " ++ show x ++ " + " ++ show y) $
    let (_, xBytecode) = astToBytecode' (AST [x]) bytecode
        (_, yBytecode) = astToBytecode' (AST [y]) bytecode
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "+"]])
astToBytecode' (MinusAST x y) bytecode = trace ("MinusAST: " ++ show x ++ " - " ++ show y) $
    let (_, xBytecode) = astToBytecode' (AST [x]) bytecode
        (_, yBytecode) = astToBytecode' (AST [y]) bytecode
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "-"]])
astToBytecode' (TimesAST x y) bytecode = trace ("TimesAST: " ++ show x ++ " * " ++ show y) $
    let (_, xBytecode) = astToBytecode' (AST [x]) bytecode
        (_, yBytecode) = astToBytecode' (AST [y]) bytecode
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "*"]])
astToBytecode' (DivideAST x y) bytecode = trace ("DivideAST: " ++ show x ++ " / " ++ show y) $
    let (_, xBytecode) = astToBytecode' (AST [x]) bytecode
        (_, yBytecode) = astToBytecode' (AST [y]) bytecode
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "/"]])
astToBytecode' (ModuloAST x y) bytecode = trace ("ModuloAST: " ++ show x ++ " % " ++ show y) $
    let (_, xBytecode) = astToBytecode' (AST [x]) bytecode
        (_, yBytecode) = astToBytecode' (AST [y]) bytecode
    in (AST [], concat [xBytecode, yBytecode, [BinaryOp "%"]])

-- * Load operations
    -- [LoadConst x, Call 1] -- system call 1 is write
-- astToBytecode' (AST [SymbolAST "exit", x]) bytecode = [LoadConst x, Call 60] -- system call 60 is exit
-- TODO LoadVar
astToBytecode' (SymbolAST x) bytecode = trace ("SymbolAST: " ++ show x) $ astToBytecode' (AST []) (bytecode ++ [LoadVar x])
astToBytecode' (IntAST x) bytecode = trace ("IntAST: " ++ show x) $ astToBytecode' (AST []) (bytecode ++ [LoadConst x])

astToBytecode' a b = trace ("Unknown AST node bytecode: " ++ show a ++ " " ++ show b) (a, b)


-- LOAD_CONST 2
-- LOAD_CONST 0
-- COMPARE_OP ==
-- JUMP_IF_FALSE 0
-- LOAD_CONST 1
-- RETURN
-- LOAD_CONST 2
-- RETURN

-- if (2 == 0) {
--     return 1;
-- } else {
--     return 2;
-- }