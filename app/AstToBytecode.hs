module AstToBytecode where

import Types
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace

-- | IfAST AST AST AST
-- | ElseIfAST AST AST AST
-- | ElseAST AST
-- | DefineAST String AST
-- | ForAST AST AST AST AST
-- | WhileAST AST AST
-- | FunTypeAST AST
-- | FunAST String AST AST AST
-- | IntTypeAST
-- | CharTypeAST
-- | StringTypeAST
-- | LambdaClosure [String] AST Environment
-- | IntAST Int
-- | SymbolAST String
-- | StringAST String
-- | CharAST Char
-- | AssignAST AST AST
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
sizeInstructionOfAst expr size = case expr of
    AST [] -> trace ("sizeInstructionOfAst AST []: " ++ show expr ++ ". Current size: " ++ show size) size
    IntAST x -> trace ("sizeInstructionOfAst IntAST: " ++ show x ++ ". Current size: " ++ show (size + 1)) (size + 1)
    SymbolAST x -> trace ("sizeInstructionOfAst SymbolAST: " ++ x ++ ". Current size: " ++ show (size + 1)) (size + 1)
    IfAST (AST cond) (AST expr1) (AST elseIfExpr1) ->
        trace ("sizeInstructionOfAst IfAST. Current size: " ++ show (size + 1)) $
        sizeInstructionOfAst (AST cond) $
        sizeInstructionOfAst (AST expr1) $
        sizeInstructionOfAst (AST elseIfExpr1) $
        size + 1
    AST (x:xs) -> trace ("sizeInstructionOfAst AST (x:xs): " ++ show x ++ ". Current size: " ++ show size) $
        sizeInstructionOfAst (AST xs) $
        sizeInstructionOfAst x $
        size
    _ -> trace ("sizeInstructionOfAst error bytecode: " ++ show expr ++ ". Current size: " ++ show size) size
sizeInstructionOfAst _ size = trace ("sizeInstructionOfAst No AST node found. Current size: " ++ show size) size
-- simpleOperation _ _ = error "Unknown AST node"



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
valueSimpleToBytecode (AST (x:xs)) = trace ("valueSimpleToBytecode AST (x:xs): " ++ show x) $
    case x of
        IntAST x -> trace ("valueSimpleToBytecode IntAST: " ++ show x) [LoadConst x]
        SymbolAST x -> trace ("valueSimpleToBytecode SymbolAST: " ++ show x) [LoadVar x]
        _ -> trace ("valueSimpleToBytecode NO AST SIMPLE NODE FOUND: " ++ show x) []
valueSimpleToBytecode x = trace ("valueSimpleToBytecode NO AST SIMPLE NODE FOUND: " ++ show x) []




-- INFO: This function takes an AST and returns a list of Bytecode instructions
--       that can be executed by the VM.
astToBytecode' :: AST -> [Bytecode] -> (AST, [Bytecode])
astToBytecode' (AST []) bytecode = (AST [], bytecode)

-- * Define Int
astToBytecode' (AST [IntTypeAST, SymbolAST x]) bytecode = trace ("AST [IntTypeAST, SymbolAST x]: " ++ show x) $ ((AST []), (bytecode ++ [StoreVar x]))

-- ! only wait for AST ReturnAST
astToBytecode' (AST [SymbolAST "return", x]) bytecode =
    let (xAST, xBytecode) = astToBytecode' (AST [x]) bytecode
    in (xAST, xBytecode ++ [Call 1]) -- system call 1 is write
-- * System calls
astToBytecode' (AST [SymbolAST "print", x]) bytecode =
    let (xAST, xBytecode) = astToBytecode' (AST [x]) bytecode
    in (xAST, xBytecode ++ [Call 1]) -- system call 1 is write
astToBytecode' (AST [SymbolAST "exit", x]) bytecode =
    let (xAST, xBytecode) = astToBytecode' (AST [x]) bytecode
    in (xAST, xBytecode ++ [Call 60]) -- system call 60 is exit

-- * (AST (x:xs))
astToBytecode' (AST (x:xs)) bytecode = trace ("Processing AST node: " ++ show x) $
    let (xAST, xBytecode) = astToBytecode' x bytecode
        (AST xsAST, xsBytecode) = astToBytecode' (AST xs) bytecode
    in (AST xsAST, xBytecode ++ xsBytecode)

astToBytecode' (IfAST cond (AST expr1) (AST elseIfExpr1)) bytecode = trace ("IfAST: " ++ show cond ++ " |expr1| " ++ show expr1 ++ " |do| " ++ show elseIfExpr1) $ do
    let condBytecode = trace ("condBytecode1: " ++ show cond) astConditionToBytecode cond bytecode
    let (expr1AST, expr1Bytecode) = trace ("expr1AST: " ++ show expr1) astToBytecode' (AST expr1) bytecode
    let (elseIfExpr1AST, elseIfExpr1Bytecode) = trace ("elseIfExpr1: " ++ show elseIfExpr1 ++ "\n\n") astToBytecode' (AST elseIfExpr1) bytecode
    (AST [], bytecode ++ condBytecode ++ [JumpIfFalse (sizeInstructionOfAst (AST expr1) 0)] ++ expr1Bytecode ++ elseIfExpr1Bytecode)

-- ! check if good behaviour
astToBytecode' (ElseAST (AST expr1)) bytecode = trace ("ElseAST: " ++ show expr1) $ do
    let (expr1AST, expr1Bytecode) = trace ("expr1AST: " ++ show expr1) astToBytecode' (AST expr1) bytecode
    (AST [], bytecode ++ expr1Bytecode)

-- * Assignation operation
astToBytecode' (AssignAST x y) bytecode = trace ("AssignAST: " ++ show x ++ " = " ++ show y) $
    let (AST yAST, yBytecode) = astToBytecode' y bytecode
        (AST xAST, xBytecode) = astToBytecode' x bytecode
    in (AST yAST, yBytecode ++ xBytecode)

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
-- LOAD_CONST 6
-- BINARY_OP +
-- LOAD_VAR a
-- STORE_VAR []
-- LOAD_VAR exit
-- LOAD_CONST 10
