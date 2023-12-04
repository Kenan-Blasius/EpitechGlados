module Types (
    File (..),
    Token (..),
    AST (..),
    indent,
    printAST
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
         | DeadLeafAST
         deriving Show


indent :: Int -> String
indent 0 = ""
indent n = "|   " ++ indent (n - 1)

printAST :: AST -> String
printAST ast = printASTIndented 0 ast
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