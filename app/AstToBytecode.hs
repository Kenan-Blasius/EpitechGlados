module AstToBytecode where

import Types
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace

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

data Bytecode = LoadConst Int
              | LoadVar String
              | StoreVar String
              | BinaryOp String
              | UnaryOp String
              | CompareOp String
              | JumpIfTrue Int
              | JumpIfFalse Int
              | Jump Int
              | Pop
              | Dup
              | Call Int
              | Return
              | BuildList Int
              | Index
              | Attribute String
              | CreateObject Int
              deriving Eq

instance Show Bytecode where
    show (LoadConst x) = "LOAD_CONST " ++ show x
    show (LoadVar x) = "LOAD_VAR " ++ x
    show (StoreVar x) = "STORE_VAR " ++ x
    show (BinaryOp x) = "BINARY_OP " ++ x
    show (UnaryOp x) = "UNARY_OP " ++ x
    show (CompareOp x) = "COMPARE_OP " ++ x
    show (JumpIfTrue x) = "JUMP_IF_TRUE " ++ show x
    show (JumpIfFalse x) = "JUMP_IF_FALSE " ++ show x
    show (Jump x) = "JUMP " ++ show x
    show Pop = "POP"
    show Dup = "DUP"
    show (Call x) = "CALL " ++ show x
    show Return = "RETURN"
    show (BuildList x) = "BUILD_LIST " ++ show x
    show Index = "INDEX"
    show (Attribute x) = "ATTRIBUTE " ++ x
    show (CreateObject x) = "CREATE_OBJECT " ++ show x


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



-- INFO: This function takes an AST and returns a list of Bytecode instructions
--       that can be executed by the VM.
astToBytecode' :: AST -> [Bytecode] -> (AST, [Bytecode])
astToBytecode' (AST []) bytecode = (AST [], bytecode)
astToBytecode' (AST (x:xs)) bytecode = trace ("Processing AST node: " ++ show x) $
    case x of
        IntAST x -> trace ("IntAST: " ++ show x) $ astToBytecode' (AST xs) (bytecode ++ [LoadConst x])
        (AST [SymbolAST "return", IntAST x']) -> trace ("return IntAST: " ++ show x') $ astToBytecode' (AST xs) (bytecode ++ [LoadConst x', Return])
        IfAST (EqualAST cond1 cond2) (AST expr1) (AST elseIfExpr1) -> trace ("IfAST: " ++ show cond1 ++ " == " ++ show cond2 ++ " " ++ show expr1 ++ " " ++ show elseIfExpr1) $ do
            let (condAST1, condBytecode1) = trace ("condAST1: " ++ show cond1) astToBytecode' cond1 bytecode
            let (condAST2, condBytecode2) = trace ("condAST2: " ++ show cond2) astToBytecode' cond2 bytecode
            let (expr1AST, expr1Bytecode) = trace ("expr1AST: " ++ show expr1) astToBytecode' (AST expr1) bytecode
            let (elseIfExpr1AST, elseIfExpr1Bytecode) = trace ("elseIfExpr1: " ++ show elseIfExpr1 ++ "\n\n") astToBytecode' (AST elseIfExpr1) bytecode
            ( AST xs, bytecode ++ condBytecode1 ++ condBytecode2 ++ [JumpIfFalse (sizeInstructionOfAst (AST expr1) 0)] ++ expr1Bytecode ++ elseIfExpr1Bytecode)
        ElseAST (AST expr1) -> trace ("ElseAST: " ++ show expr1) $ do
            let (expr1AST, expr1Bytecode) = trace ("expr1AST: " ++ show expr1) astToBytecode' (AST expr1) bytecode
            (AST xs, bytecode ++ expr1Bytecode)
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
