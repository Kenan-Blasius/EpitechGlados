module Types (
    ParserError (..),
    File (..),
    Token (..),
    AST (..),
    Environment,
    indent,
    printAST,
    Bytecode (..),
    DataType(..)
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
            | ElseIfToken
            | ElseToken
            | ForToken
            | WhileToken
            | FunToken
            | FunTypeToken
            | IntTypeToken
            | FloatTypeToken
            | CharTypeToken
            | StringTypeToken
            | VoidTypeToken
            | OpenBracket
            | CloseBracket
            | OpenBraces
            | CloseBraces
            -- | CommentStart
            -- | CommentEnd
            -- | InlineCommentStart
            | DefineToken
            | IncludeToken
            | IntToken Int
            | FloatToken Float
            | SymbolToken String
            | StringToken String
            | CharToken Char
            | CommaToken
            | ReturnToken
            | LineSeparator

            | AssignToken
            | IncrementToken
            | DecrementToken
            | PlusEqualToken
            | MinusEqualToken
            | TimesEqualToken
            | DivideEqualToken
            | ModuloEqualToken
            | PlusToken
            | MinusToken
            | TimesToken
            | DivideToken
            | ModuloToken
            | AndToken
            | OrToken
            | NotToken
            | EqualToken
            | NotEqualToken
            | LessThanToken
            | LessThanEqualToken
            | GreaterThanToken
            | GreaterThanEqualToken

            | ListToken [Token]
            -- deriving Show

instance Show Token where
    show OpenParenthesis = "OpenPARENTHESIS"
    show CloseParenthesis = "ClosePARENTHESIS"
    show SpaceToken = "SPACE"
    show IfToken = "IF"
    show ElseIfToken = "ELSEIF"
    show ElseToken = "ELSE"
    show ForToken = "FOR"
    show WhileToken = "WHILE"
    show FunToken = "FUN"
    show FunTypeToken = "FUNTYPE"
    show IntTypeToken = "INT"
    show FloatTypeToken = "FLOAT"
    show CharTypeToken = "CHAR"
    show StringTypeToken = "STRING"
    show VoidTypeToken = "VOID"
    show OpenBracket = "OpenBRACKET"
    show CloseBracket = "CloseBRACKET"
    show OpenBraces = "OpenBRACES"
    show CloseBraces = "CloseBRACES"
    -- show CommentStart = "/*"
    -- show CommentEnd = "*/"
    -- show InlineCommentStart = "//"
    show DefineToken = "DEFINE"
    show IncludeToken = "INCLUDE"
    show (IntToken x) = show x
    show (FloatToken x) = show x
    show (SymbolToken x) = x
    show (StringToken x) = "\"" ++ x ++ "\""
    show (CharToken x) = show x
    show CommaToken = "COMMA"
    show ReturnToken = "RETURN"
    show LineSeparator = "LineSEPARATOR"

    show AssignToken = "Assign"
    show IncrementToken = "Increment"
    show DecrementToken = "Decrement"
    show PlusEqualToken = "PlusEqual"
    show MinusEqualToken = "MinusEqual"
    show TimesEqualToken = "TimesEqual"
    show DivideEqualToken = "DivideEqual"
    show ModuloEqualToken = "ModuloEqual"
    show PlusToken = "Plus"
    show MinusToken = "Minus"
    show TimesToken = "Times"
    show DivideToken = "Divide"
    show ModuloToken = "Modulo"
    show AndToken = "And"
    show OrToken = "Or"
    show NotToken = "Not"
    show EqualToken = "Equal"
    show NotEqualToken = "NotEqual"
    show LessThanToken = "LessThan"
    show LessThanEqualToken = "LessThanEqual"
    show GreaterThanToken = "GreaterThan"
    show GreaterThanEqualToken = "GreaterThanEqual"

    show (ListToken x) = show x

instance Eq Token where
    OpenParenthesis == OpenParenthesis = True
    CloseParenthesis == CloseParenthesis = True
    SpaceToken == SpaceToken = True
    IfToken == IfToken = True
    ElseIfToken == ElseIfToken = True
    ElseToken == ElseToken = True
    ForToken == ForToken = True
    WhileToken == WhileToken = True
    FunToken == FunToken = True
    FunTypeToken == FunTypeToken = True
    IntTypeToken == IntTypeToken = True
    FloatTypeToken == FloatTypeToken = True
    CharTypeToken == CharTypeToken = True
    StringTypeToken == StringTypeToken = True
    VoidTypeToken == VoidTypeToken = True
    OpenBracket == OpenBracket = True
    CloseBracket == CloseBracket = True
    OpenBraces == OpenBraces = True
    CloseBraces == CloseBraces = True
    -- CommentStart == CommentStart = True
    -- CommentEnd == CommentEnd = True
    -- InlineCommentStart == InlineCommentStart = True
    DefineToken == DefineToken = True
    IncludeToken == IncludeToken = True
    (IntToken x) == (IntToken y) = x == y
    (FloatToken x) == (FloatToken y) = x == y
    (SymbolToken x) == (SymbolToken y) = x == y
    (StringToken x) == (StringToken y) = x == y
    (CharToken x) == (CharToken y) = x == y
    CommaToken == CommaToken = True
    ReturnToken == ReturnToken = True
    LineSeparator == LineSeparator = True

    AssignToken == AssignToken = True
    IncrementToken == IncrementToken = True
    DecrementToken == DecrementToken = True
    PlusEqualToken == PlusEqualToken = True
    MinusEqualToken == MinusEqualToken = True
    TimesEqualToken == TimesEqualToken = True
    DivideEqualToken == DivideEqualToken = True
    ModuloEqualToken == ModuloEqualToken = True
    PlusToken == PlusToken = True
    MinusToken == MinusToken = True
    TimesToken == TimesToken = True
    DivideToken == DivideToken = True
    ModuloToken == ModuloToken = True
    AndToken == AndToken = True
    OrToken == OrToken = True
    NotToken == NotToken = True
    EqualToken == EqualToken = True
    NotEqualToken == NotEqualToken = True
    LessThanToken == LessThanToken = True
    LessThanEqualToken == LessThanEqualToken = True
    GreaterThanToken == GreaterThanToken = True
    GreaterThanEqualToken == GreaterThanEqualToken = True

    (ListToken x) == (ListToken y) = x == y
    _ == _ = False

-- All AST Types

data AST = AST [AST] -- list of AST
         | IfAST AST AST AST -- cond expr1 elseIfExpr/elseExpr
         | ElseIfAST AST AST AST -- cond expr elseIfExpr/elseExpr
         | ElseAST AST -- expr
        --  | DefineAST String AST -- name expr
         | ForAST AST AST AST AST -- init cond incr expr
         | WhileAST AST AST -- cond expr
         | FunTypeAST AST -- type
         | FunAST String AST AST AST -- name arg returnType expr
         | ReturnAST AST -- expr
         | IntTypeAST -- type
         | FloatTypeAST -- type
         | CharTypeAST -- type
         | StringTypeAST -- type
         | VoidTypeAST -- type
         | LambdaClosure [String] AST Environment
         | IntAST Int -- value
         | FloatAST Float -- value
         | SymbolAST String -- name
         | StringAST String -- value
         | CharAST Char -- value

            -- =
         | AssignAST AST AST -- left right
            -- ==
         | EqualAST AST AST -- left right
            -- <
         | LessThanAST AST AST -- left right
            -- >
         | GreaterThanAST AST AST -- left right
            -- <=
         | LessThanEqualAST AST AST -- left right
            -- >=
         | GreaterThanEqualAST AST AST -- left right
            -- !=
         | NotEqualAST AST AST -- left right
            -- +
         | PlusAST AST AST -- left right
            -- -
         | MinusAST AST AST -- left right
            -- *
         | TimesAST AST AST -- left right
            -- /
         | DivideAST AST AST -- left right
            -- %
         | ModuloAST AST AST -- left right
            -- &&
         | AndAST AST AST -- left right
            -- ||
         | OrAST AST AST -- left right
            -- +=
         | PlusEqualAST AST AST -- left right
            -- -=
         | MinusEqualAST AST AST -- left right
            -- *=
         | TimesEqualAST AST AST -- left right
            -- /=
         | DivideEqualAST AST AST -- left right
            -- %=
         | ModuloEqualAST AST AST -- left right
            -- !
         | NotAST AST -- expr
            -- ++
         | IncrementAST AST -- left
            -- --
         | DecrementAST AST -- left

         | DeadLeafAST
         deriving Show

instance Eq AST where
    AST x == AST y = x == y
    IfAST cond1 expr1 elseIfExpr1 == IfAST cond2 expr2 elseIfExpr2 = cond1 == cond2 && expr1 == expr2 && elseIfExpr1 == elseIfExpr2
    ElseIfAST cond1 expr1 elseIfExpr1 == ElseIfAST cond2 expr2 elseIfExpr2 = cond1 == cond2 && expr1 == expr2 && elseIfExpr1 == elseIfExpr2
    ElseAST expr1 == ElseAST expr2 = expr1 == expr2
    -- DefineAST name1 expr1 == DefineAST name2 expr2 = name1 == name2 && expr1 == expr2
    ForAST init1 cond1 incr1 expr1 == ForAST init2 cond2 incr2 expr2 = init1 == init2 && cond1 == cond2 && incr1 == incr2 && expr1 == expr2
    WhileAST cond1 expr1 == WhileAST cond2 expr2 = cond1 == cond2 && expr1 == expr2
    FunTypeAST type1 == FunTypeAST type2 = type1 == type2
    FunAST name1 arg1 type1 expr1 == FunAST name2 arg2 type2 expr2 = name1 == name2 && arg1 == arg2 && type1 == type2 && expr1 == expr2
    ReturnAST expr1 == ReturnAST expr2 = expr1 == expr2
    IntTypeAST == IntTypeAST = True
    FloatTypeAST == FloatTypeAST = True
    CharTypeAST == CharTypeAST = True
    StringTypeAST == StringTypeAST = True
    VoidTypeAST == VoidTypeAST = True
    LambdaClosure args1 body1 env1 == LambdaClosure args2 body2 env2 = args1 == args2 && body1 == body2 && env1 == env2
    IntAST x == IntAST y = x == y
    FloatAST x == FloatAST y = x == y
    SymbolAST x == SymbolAST y = x == y
    StringAST x == StringAST y = x == y
    CharAST x == CharAST y = x == y

    AssignAST left1 right1 == AssignAST left2 right2 = left1 == left2 && right1 == right2
    EqualAST left1 right1 == EqualAST left2 right2 = left1 == left2 && right1 == right2
    LessThanAST left1 right1 == LessThanAST left2 right2 = left1 == left2 && right1 == right2
    GreaterThanAST left1 right1 == GreaterThanAST left2 right2 = left1 == left2 && right1 == right2
    LessThanEqualAST left1 right1 == LessThanEqualAST left2 right2 = left1 == left2 && right1 == right2
    GreaterThanEqualAST left1 right1 == GreaterThanEqualAST left2 right2 = left1 == left2 && right1 == right2
    NotEqualAST left1 right1 == NotEqualAST left2 right2 = left1 == left2 && right1 == right2
    PlusAST left1 right1 == PlusAST left2 right2 = left1 == left2 && right1 == right2
    MinusAST left1 right1 == MinusAST left2 right2 = left1 == left2 && right1 == right2
    TimesAST left1 right1 == TimesAST left2 right2 = left1 == left2 && right1 == right2
    DivideAST left1 right1 == DivideAST left2 right2 = left1 == left2 && right1 == right2
    ModuloAST left1 right1 == ModuloAST left2 right2 = left1 == left2 && right1 == right2
    AndAST left1 right1 == AndAST left2 right2 = left1 == left2 && right1 == right2
    OrAST left1 right1 == OrAST left2 right2 = left1 == left2 && right1 == right2
    PlusEqualAST left1 right1 == PlusEqualAST left2 right2 = left1 == left2 && right1 == right2
    MinusEqualAST left1 right1 == MinusEqualAST left2 right2 = left1 == left2 && right1 == right2
    TimesEqualAST left1 right1 == TimesEqualAST left2 right2 = left1 == left2 && right1 == right2
    DivideEqualAST left1 right1 == DivideEqualAST left2 right2 = left1 == left2 && right1 == right2
    ModuloEqualAST left1 right1 == ModuloEqualAST left2 right2 = left1 == left2 && right1 == right2
    NotAST expr1 == NotAST expr2 = expr1 == expr2
    IncrementAST left1 == IncrementAST left2 = left1 == left2
    DecrementAST left1 == DecrementAST left2 = left1 == left2

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
        printASTIndented depth (FloatAST value) = indent depth ++ "FloatAST " ++ show value ++ "\n"
        printASTIndented depth (SymbolAST name) = indent depth ++ "SymbolAST " ++ name ++ "\n"
        -- printASTIndented depth (DefineAST name expr) =
        --     indent depth ++ "DefineAST " ++ name ++ "\n" ++ printASTIndented (depth + 1) expr
        printASTIndented depth (IfAST cond expr1 elseIfExpr) =
            indent depth ++ "IfAST\n" ++
                printASTIndented (depth + 1) cond ++
                printASTIndented (depth + 1) expr1 ++
                printASTIndented (depth + 1) elseIfExpr
        printASTIndented depth (AST astList) =
            indent depth ++ "AST\n" ++ concatMap (printASTIndented (depth + 1)) astList
        printASTIndented depth (LambdaClosure args body env) =
            indent depth ++ "LambdaClosure\n" ++
                indent (depth + 1) ++ "args: " ++ show args ++ "\n" ++
                indent (depth + 1) ++ "body: " ++ printASTIndented (depth + 1) body ++ "\n" ++
                indent (depth + 1) ++ "env: " ++ show env ++ "\n"
        printASTIndented depth (FunTypeAST typeAST) =
            indent depth ++ "FunTypeAST\n" ++ printASTIndented (depth + 1) typeAST
        printASTIndented depth (FunAST name arg returnType expr) =
            indent depth ++ "FunAST " ++ name ++ "\n" ++
                printASTIndented (depth + 1) arg ++
                printASTIndented (depth + 1) returnType ++
                printASTIndented (depth + 1) expr
        printASTIndented depth (ElseAST expr) =
            indent depth ++ "ElseAST\n" ++ printASTIndented (depth + 1) expr
        printASTIndented depth IntTypeAST = indent depth ++ "IntTypeAST\n"
        printASTIndented depth FloatTypeAST = indent depth ++ "FloatTypeAST\n"
        printASTIndented depth CharTypeAST = indent depth ++ "CharTypeAST\n"
        printASTIndented depth StringTypeAST = indent depth ++ "StringTypeAST\n"
        printASTIndented depth VoidTypeAST = indent depth ++ "VoidTypeAST\n"
        printASTIndented depth (StringAST value) = indent depth ++ "StringAST " ++ value ++ "\n"
        printASTIndented depth (CharAST value) = indent depth ++ "CharAST '" ++ [value] ++ "'\n"
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
        printASTIndented depth (ElseIfAST cond expr elseIfExpr) =
            indent depth ++ "ElseIfAST\n" ++
                printASTIndented (depth + 1) cond ++
                printASTIndented (depth + 1) expr ++
                printASTIndented (depth + 1) elseIfExpr
        printASTIndented depth (ReturnAST expr) =
            indent depth ++ "ReturnAST\n" ++ printASTIndented (depth + 1) expr

        printASTIndented depth (AssignAST left right) =
            indent depth ++ "AssignAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (EqualAST left right) =
            indent depth ++ "EqualAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (LessThanAST left right) =
            indent depth ++ "LessThanAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (GreaterThanAST left right) =
            indent depth ++ "GreaterThanAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (LessThanEqualAST left right) =
            indent depth ++ "LessThanEqualAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (GreaterThanEqualAST left right) =
            indent depth ++ "GreaterThanEqualAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (NotEqualAST left right) =
            indent depth ++ "NotEqualAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (PlusAST left right) =
            indent depth ++ "PlusAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (MinusAST left right) =
            indent depth ++ "MinusAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (TimesAST left right) =
            indent depth ++ "TimesAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (DivideAST left right) =
            indent depth ++ "DivideAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (ModuloAST left right) =
            indent depth ++ "ModuloAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (AndAST left right) =
            indent depth ++ "AndAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (OrAST left right) =
            indent depth ++ "OrAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (PlusEqualAST left right) =
            indent depth ++ "PlusEqualAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (MinusEqualAST left right) =
            indent depth ++ "MinusEqualAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (TimesEqualAST left right) =
            indent depth ++ "TimesEqualAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (DivideEqualAST left right) =
            indent depth ++ "DivideEqualAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (ModuloEqualAST left right) =
            indent depth ++ "ModuloEqualAST\n" ++
                printASTIndented (depth + 1) left ++
                printASTIndented (depth + 1) right
        printASTIndented depth (NotAST expr) =
            indent depth ++ "NotAST\n" ++ printASTIndented (depth + 1) expr
        printASTIndented depth (IncrementAST left) =
            indent depth ++ "IncrementAST\n" ++ printASTIndented (depth + 1) left
        printASTIndented depth (DecrementAST left) =
            indent depth ++ "DecrementAST\n" ++ printASTIndented (depth + 1) left

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



data DataType =  IntType
                | FloatType
                | StringType
                | CharType
                | FunType
                | VoidType
                | BoolType
                | UnknownType
                deriving (Show, Eq)

data Bytecode = LoadConst Int DataType
              | LoadVarBefore String DataType
              | StoreVarBefore String DataType
              | LoadVar Int DataType
              | StoreVar Int DataType
              | BinaryOp String -- ? DataType
              | UnaryOp String -- ? DataType
              | CompareOp String -- ? DataType
              | JumpIfTrue Int
              | JumpIfFalse Int
              | Jump Int
              | JumpNewScope Int
              | JumpIfTrueBefore Int
              | JumpIfFalseBefore Int
              | JumpBefore Int
              | JumpRef Int
              | Pop
              | Dup
              | Call Int
              | Return
              | FunEntryPoint String DataType
              | CallUserFun String
              | LoadPC
              | StringToSave String
              | Index
              | SaveAt
            --   | BuildList Int
            --   | ListAppend            -- No additional values needed
            --   | ListConcat            -- No additional values needed
            --   | ListSlice Int Int     -- Requires two Int values (start and end indices)
            --   | ListLength            -- No additional values needed
            --   | ListPop Int           -- Requires an Int value (index from which to pop)
            --   | ListInsert Int        -- Requires two values - an Int (index) and a value to insert
            --  ? | PushFrame
            --  ? | PopFrame
            -- * Unused, but could be useful in the future
              deriving Eq

instance Show Bytecode where
    show (LoadConst x y) =    "LoadConst " ++ show x ++ " " ++ show y
    show (LoadVarBefore x y) =  "LoadVarBefore \""  ++ x ++ "\" " ++ show y
    show (StoreVarBefore x y) = "StoreVarBefore \"" ++ x ++ "\" " ++ show y
    show (LoadVar x y) =      "LoadVar "  ++ show x ++ " " ++ show y
    show (StoreVar x y) =     "StoreVar " ++ show x ++ " " ++ show y
    show (BinaryOp x) =     "BinaryOp \""  ++ x ++ "\""
    show (UnaryOp x) =      "UnaryOp \""   ++ x ++ "\""
    show (CompareOp x) =    "CompareOp \"" ++ x ++ "\""
    show (JumpIfTrue x) =   "JumpIfTrue "  ++ show x
    show (JumpIfFalse x) =  "JumpIfFalse " ++ show x
    show (Jump x) =         "Jump " ++ show x
    show (JumpNewScope x) = "JumpNewScope " ++ show x ++ " "
    show (JumpIfTrueBefore x) =   "JumpIfTrueBefore "  ++ show x
    show (JumpIfFalseBefore x) =  "JumpIfFalseBefore " ++ show x
    show (JumpBefore x) =         "JumpBefore " ++ show x
    show (JumpRef x) =      "JumpRef " ++ show x
    show Pop =              "Pop"
    show Dup =              "Dup"
    show (Call x) =         "Call " ++ show x
    show Return =           "Return"
    show (FunEntryPoint x y) = "FunEntryPoint " ++ show x ++ " " ++ show y
    show (CallUserFun x) =  "CallUserFun " ++ show x
    show LoadPC =           "LoadPC"
    show (StringToSave x) = "StringToSave \"" ++ show x ++ "\""
    show Index =            "INDEX"
    show SaveAt =           "SAVE_AT"
    -- ? show PushFrame =        "PUSH_FRAME"
    -- ? show PopFrame =         "POP_FRAME"
    -- show (BuildList x) =    "BUILD_LIST " ++ show x

