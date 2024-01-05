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


exploreAllSymbols :: String -> Int -> [Bytecode]
exploreAllSymbols "print" x = [LoadConst x, Call 1] -- system call 1 is write
exploreAllSymbols "exit" x = [LoadConst x, Call 60] -- system call 60 is exit





-- INFO: This function takes an AST and returns a list of Bytecode instructions
--       that can be executed by the VM.
astToBytecode' :: AST -> [Bytecode] -> (AST, [Bytecode])
astToBytecode' (AST []) bytecode = (AST [], bytecode)
astToBytecode' (AST (x:xs)) bytecode = trace ("Processing AST node: " ++ show x) $
    case x of
        IntAST x -> trace ("IntAST: " ++ show x) $ astToBytecode' (AST xs) (bytecode ++ [LoadConst x])
        (AST [SymbolAST "return", IntAST x']) -> trace ("return IntAST: " ++ show x') $ astToBytecode' (AST xs) (bytecode ++ [LoadConst x', Return]) -- ! will change t ReturnAST
        IfAST cond (AST expr1) (AST elseIfExpr1) -> trace ("IfAST: " ++ show cond ++ " |expr1| " ++ show expr1 ++ " |do| " ++ show elseIfExpr1) $ do
            let condBytecode = trace ("condBytecode1: " ++ show cond) astConditionToBytecode cond bytecode
            let (expr1AST, expr1Bytecode) = trace ("expr1AST: " ++ show expr1) astToBytecode' (AST expr1) bytecode
            let (elseIfExpr1AST, elseIfExpr1Bytecode) = trace ("elseIfExpr1: " ++ show elseIfExpr1 ++ "\n\n") astToBytecode' (AST elseIfExpr1) bytecode
            (AST xs, bytecode ++ condBytecode ++ [JumpIfFalse (sizeInstructionOfAst (AST expr1) 0)] ++ expr1Bytecode ++ elseIfExpr1Bytecode)
        ElseAST (AST expr1) -> trace ("ElseAST: " ++ show expr1) $ do
            let (expr1AST, expr1Bytecode) = trace ("expr1AST: " ++ show expr1) astToBytecode' (AST expr1) bytecode
            (AST xs, bytecode ++ expr1Bytecode)

        AssignAST (AST [IntTypeAST, SymbolAST x]) (AST [IntAST y]) ->
            trace ("AssignAST: " ++ show x ++ " = " ++ show y)
            $ astToBytecode' (AST xs) (bytecode ++ [LoadConst y, StoreVar x]) -- do this in several steps if several values to assign ( int a = 1 + 1 )

        (AST [SymbolAST x, AST [IntAST y]]) ->
            trace ("call exploreAllSymbols: " ++ show x ++ " = " ++ show y)
            $ astToBytecode' (AST xs) (bytecode ++ exploreAllSymbols x y)

        -- SymbolAST x -> trace ("SymbolAST: " ++ show x) $ astToBytecode' (AST xs) (bytecode ++ [LoadVar x])

        _ -> trace ("AGAIN astToBytecode' (AST xs) bytecode" ++ show x) astToBytecode' (AST xs) bytecode

astToBytecode' (PlusAST (AST [IntAST x]) (AST [IntAST y])) bytecode = trace ("add PlusAST to the bytecode: " ++ show x ++ " + " ++ show y) $
    astToBytecode' (AST []) (bytecode ++ [LoadConst (x) , LoadConst (y), BinaryOp "+"])
astToBytecode' (MinusAST (AST [IntAST x]) (AST [IntAST y])) bytecode = trace ("MinusAST: " ++ show x ++ " - " ++ show y) $
    astToBytecode' (AST []) (bytecode ++ [LoadConst (x) , LoadConst (y), BinaryOp "-"])
astToBytecode' (TimesAST (AST [IntAST x]) (AST [IntAST y])) bytecode = trace ("TimesAST: " ++ show x ++ " * " ++ show y) $
    astToBytecode' (AST []) (bytecode ++ [LoadConst (x) , LoadConst (y), BinaryOp "*"])
astToBytecode' (DivideAST (AST [IntAST x]) (AST [IntAST y])) bytecode = trace ("DivideAST: " ++ show x ++ " / " ++ show y) $
    astToBytecode' (AST []) (bytecode ++ [LoadConst (x) , LoadConst (y), BinaryOp "/"])
astToBytecode' (ModuloAST (AST [IntAST x]) (AST [IntAST y])) bytecode = trace ("ModuloAST: " ++ show x ++ " % " ++ show y) $
    astToBytecode' (AST []) (bytecode ++ [LoadConst (x) , LoadConst (y), BinaryOp "%"])
astToBytecode' (SymbolAST x) bytecode = trace ("SymbolAST: " ++ show x) $ astToBytecode' (AST []) (bytecode ++ [LoadVar x])

astToBytecode' a b = trace ("Unknown AST node bytecode: " ++ show a ++ " " ++ show b) (a, b)
