module Types (
    File (..),
    Token (..),
    AST (..)
) where

-- File type
data File = File [String]

instance Show File where
    show (File []) = ""
    show (File (x:xs)) = x ++ "\n" ++ show (File xs)

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

-- All AST Types
data AST = AST [AST] -- list of AST
         | IfAST AST AST AST -- cond expr1 expr2
         | DefineAST String AST -- name expr
         | LambdaAST AST AST -- args body
         | IntAST Int -- value
         | SymbolAST String -- name
         deriving Show