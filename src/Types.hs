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
            | ElseToken
            | ForToken
            | WhileToken
            | FunToken
            | FunTypeToken
            | IntTypeToken
            | CharTypeToken
            | StringTypeToken
            | OpenBracket
            | CloseBracket
            | OpenBraces
            | CloseBraces
            | CommentStart
            | CommentEnd
            | InlineCommentStart
            | DefineToken
            | LambdaToken
            | IntToken Int
            | SymbolToken String
            | StringToken String
            | CharToken Char
            | CommaToken
            | LineSeparator
            | ListToken [Token]
            -- deriving Show

instance Show Token where
    show OpenParenthesis = "OpenPARENTHESIS"
    show CloseParenthesis = "ClosePARENTHESIS"
    show SpaceToken = "SPACE"
    show IfToken = "IF"
    show ElseToken = "ELSE"
    show ForToken = "FOR"
    show WhileToken = "WHILE"
    show FunToken = "FUN"
    show FunTypeToken = "FUNTYPE"
    show IntTypeToken = "INT"
    show CharTypeToken = "CHAR"
    show StringTypeToken = "STRING"
    show OpenBracket = "OpenBRACKET"
    show CloseBracket = "CloseBRACKET"
    show OpenBraces = "OpenBRACES"
    show CloseBraces = "CloseBRACES"
    show CommentStart = "/*"
    show CommentEnd = "*/"
    show InlineCommentStart = "//"
    show DefineToken = "DEFINE"
    show LambdaToken = "LAMBDA"
    show (IntToken x) = show x
    show (SymbolToken x) = x
    show (StringToken x) = "\"" ++ x ++ "\""
    show (CharToken x) = show x
    show CommaToken = "COMMA"
    show LineSeparator = "LineSEPARATOR"
    show (ListToken x) = show x

instance Eq Token where
    OpenParenthesis == OpenParenthesis = True
    CloseParenthesis == CloseParenthesis = True
    SpaceToken == SpaceToken = True
    IfToken == IfToken = True
    ElseToken == ElseToken = True
    ForToken == ForToken = True
    WhileToken == WhileToken = True
    FunToken == FunToken = True
    FunTypeToken == FunTypeToken = True
    IntTypeToken == IntTypeToken = True
    CharTypeToken == CharTypeToken = True
    StringTypeToken == StringTypeToken = True
    OpenBracket == OpenBracket = True
    CloseBracket == CloseBracket = True
    OpenBraces == OpenBraces = True
    CloseBraces == CloseBraces = True
    CommentStart == CommentStart = True
    CommentEnd == CommentEnd = True
    InlineCommentStart == InlineCommentStart = True
    DefineToken == DefineToken = True
    LambdaToken == LambdaToken = True
    (IntToken x) == (IntToken y) = x == y
    (SymbolToken x) == (SymbolToken y) = x == y
    (StringToken x) == (StringToken y) = x == y
    (CharToken x) == (CharToken y) = x == y
    CommaToken == CommaToken = True
    LineSeparator == LineSeparator = True
    (ListToken x) == (ListToken y) = x == y
    _ == _ = False

-- All AST Types

data AST = AST [AST] -- list of AST
         | IfAST AST AST -- cond expr1
         | ElseAST AST -- expr
         | DefineAST String AST -- name expr
         | ForAST AST AST AST AST -- init cond incr expr
         | WhileAST AST AST -- cond expr
         | FunTypeAST AST -- type
         | FunAST String AST AST AST -- name returnType arg expr
         | IntTypeAST -- type
         | CharTypeAST -- type
         | StringTypeAST -- type
         | LambdaAST AST AST -- args body
         | LambdaClosure [String] AST Environment
         | IntAST Int -- value
         | SymbolAST String -- name
         | StringAST String -- value
         | CharAST Char -- value
         | DeadLeafAST
         deriving Show

instance Eq AST where
    AST x == AST y = x == y
    IfAST cond1 expr1 == IfAST cond2 expr3 = cond1 == cond2 && expr1 == expr3
    ElseAST expr1 == ElseAST expr2 = expr1 == expr2
    DefineAST name1 expr1 == DefineAST name2 expr2 = name1 == name2 && expr1 == expr2
    ForAST init1 cond1 incr1 expr1 == ForAST init2 cond2 incr2 expr2 = init1 == init2 && cond1 == cond2 && incr1 == incr2 && expr1 == expr2
    WhileAST cond1 expr1 == WhileAST cond2 expr2 = cond1 == cond2 && expr1 == expr2
    FunAST name1 type1 arg1 expr1 == FunAST name2 type2 arg2 expr2 = name1 == name2 && type1 == type2 && arg1 == arg2 && expr1 == expr2
    IntTypeAST == IntTypeAST = True
    CharTypeAST == CharTypeAST = True
    StringTypeAST == StringTypeAST = True
    LambdaAST args1 body1 == LambdaAST args2 body2 = args1 == args2 && body1 == body2
    LambdaClosure args1 body1 env1 == LambdaClosure args2 body2 env2 = args1 == args2 && body1 == body2 && env1 == env2
    IntAST x == IntAST y = x == y
    SymbolAST x == SymbolAST y = x == y
    StringAST x == StringAST y = x == y
    CharAST x == CharAST y = x == y
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
        printASTIndented depth (IfAST cond expr1) =
            indent depth ++ "IfAST\n" ++
                printASTIndented (depth + 1) cond ++
                printASTIndented (depth + 1) expr1
        printASTIndented depth (AST astList) =
            indent depth ++ "AST\n" ++ concatMap (printASTIndented (depth + 1)) astList
        printASTIndented depth (LambdaClosure args body env) =
            indent depth ++ "LambdaClosure\n" ++
                indent (depth + 1) ++ "args: " ++ show args ++ "\n" ++
                indent (depth + 1) ++ "body: " ++ printASTIndented (depth + 1) body ++ "\n" ++
                indent (depth + 1) ++ "env: " ++ show env ++ "\n"
        printASTIndented depth (FunTypeAST typeAST) =
            indent depth ++ "FunTypeAST\n" ++ printASTIndented (depth + 1) typeAST
        printASTIndented depth (FunAST name returnType arg expr) =
            indent depth ++ "FunAST " ++ name ++ "\n" ++
                printASTIndented (depth + 1) returnType ++
                printASTIndented (depth + 1) arg ++
                printASTIndented (depth + 1) expr
        printASTIndented depth (ElseAST expr) =
            indent depth ++ "ElseAST\n" ++ printASTIndented (depth + 1) expr
        printASTIndented depth IntTypeAST = indent depth ++ "IntTypeAST\n"
        printASTIndented depth CharTypeAST = indent depth ++ "CharTypeAST\n"
        printASTIndented depth StringTypeAST = indent depth ++ "StringTypeAST\n"
        printASTIndented depth (StringAST value) = indent depth ++ "StringAST " ++ show value ++ "\n"
        printASTIndented depth (CharAST value) = indent depth ++ "CharAST " ++ show value ++ "\n"
        printASTIndented depth (ForAST initer cond incr expr) =
            indent depth ++ "ForAST\n" ++
                printASTIndented (depth + 1) initer ++
                printASTIndented (depth + 1) cond ++
                printASTIndented (depth + 1) incr ++
                printASTIndented (depth + 1) expr
        printASTIndented depth (WhileAST cond expr) =
            indent depth ++ "WhileAST\n" ++
                printASTIndented (depth + 1) cond ++
                printASTIndented (depth + 1) expr

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
