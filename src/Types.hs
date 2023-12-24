module Types (
    ParserError (..),
    File (..),
    Token (..),
    AST (..),
    Environment,
    indent,
    printAST
) where

import Control.Exception
-- import Control.Monad.State

-- File type
newtype File = File [String]

instance Show File where
    show (File []) = ""
    show (File (x:xs)) = x ++ "\n" ++ show (File xs)

type Environment = [(String, AST)]

-- Parser Error Type
data ParserError = ParserError String

instance Show ParserError where
    show (ParserError x) = x

instance Exception ParserError

-- All Tokens Types
data Token = OpenParenthesis
            | CloseParenthesis
            | SpaceToken
            | IfToken
            | DefineToken
            | LambdaToken
            | IntToken Int
            | SymbolToken String
            | ListToken [Token]
            -- deriving Show

instance Show Token where
    show OpenParenthesis = "OpenPARENTHESIS"
    show CloseParenthesis = "ClosePARENTHESIS"
    show SpaceToken = "SPACE"
    show IfToken = "IF"
    show DefineToken = "DEFINE"
    show LambdaToken = "LAMBDA"
    show (IntToken x) = show x
    show (SymbolToken x) = x
    show (ListToken x) = show x

instance Eq Token where
    OpenParenthesis == OpenParenthesis = True
    CloseParenthesis == CloseParenthesis = True
    SpaceToken == SpaceToken = True
    IfToken == IfToken = True
    DefineToken == DefineToken = True
    LambdaToken == LambdaToken = True
    (IntToken x) == (IntToken y) = x == y
    (SymbolToken x) == (SymbolToken y) = x == y
    (ListToken x) == (ListToken y) = x == y
    _ == _ = False

-- All AST Types
data AST = AST [AST] -- list of AST
         | IfAST AST AST AST -- cond expr1 expr2
         | DefineAST String AST -- name expr
         | LambdaAST AST AST -- args body
         | LambdaClosure [String] AST Environment
         | IntAST Int -- value
         | SymbolAST String -- name
         | DeadLeafAST
         deriving Show

instance Eq AST where
    AST x == AST y = x == y
    IfAST cond1 expr1 expr2 == IfAST cond2 expr3 expr4 = cond1 == cond2 && expr1 == expr3 && expr2 == expr4
    DefineAST name1 expr1 == DefineAST name2 expr2 = name1 == name2 && expr1 == expr2
    LambdaAST args1 body1 == LambdaAST args2 body2 = args1 == args2 && body1 == body2
    LambdaClosure args1 body1 env1 == LambdaClosure args2 body2 env2 = args1 == args2 && body1 == body2 && env1 == env2
    IntAST x == IntAST y = x == y
    SymbolAST x == SymbolAST y = x == y
    DeadLeafAST == DeadLeafAST = True
    _ == _ = False

indent :: Int -> String
indent 0 = ""
indent n = "|   " ++ indent (n - 1)

printAST :: AST -> String
printAST = printASTIndented 0
    where
        printASTIndented :: Int -> AST -> String
        printASTIndented depth DeadLeafAST = indent depth ++ "DeadLeafAST\n"
        printASTIndented depth (IntAST value) = indent depth ++ "IntAST " ++ show value ++ "\n"
        printASTIndented depth (SymbolAST name) = indent depth ++ "SymbolAST " ++ name ++ "\n"
        printASTIndented depth (DefineAST name expr) =
            indent depth ++ "DefineAST " ++ name ++ "\n" ++ printASTIndented (depth + 1) expr
        printASTIndented depth (LambdaAST args body) =
            indent depth ++ "LambdaAST\n" ++ printASTIndented (depth + 1) args ++ printASTIndented (depth + 1) body
        printASTIndented depth (IfAST cond expr1 expr2) =
            indent depth ++ "IfAST\n" ++
                printASTIndented (depth + 1) cond ++
                printASTIndented (depth + 1) expr1 ++
                printASTIndented (depth + 1) expr2
        printASTIndented depth (AST astList) =
            indent depth ++ "AST\n" ++ concatMap (printASTIndented (depth + 1)) astList
        printASTIndented depth (LambdaClosure args body env) =
            indent depth ++ "LambdaClosure\n" ++
                indent (depth + 1) ++ "args: " ++ show args ++ "\n" ++
                indent (depth + 1) ++ "body: " ++ printASTIndented (depth + 1) body ++ "\n" ++
                indent (depth + 1) ++ "env: " ++ show env ++ "\n"

-- Overload the ++ operator for AST
instance Semigroup AST where
    -- Adding a dead leaf to an AST is the same as adding nothing
    DeadLeafAST <> x = x
    x <> DeadLeafAST = x
    -- Adding two AST is the same as adding a list of AST
    AST x <> AST y = AST (x ++ y)
    AST x <> y = AST (x ++ [y])
    x <> AST y = AST (x : y)
    x <> y = AST [x, y]
