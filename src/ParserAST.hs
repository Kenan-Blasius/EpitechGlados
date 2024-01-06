module ParserAST (
    splitAtValue,
    splitAtLastValue,

    getIfChain,

    binaryOperatorsAST,
    operatorsAfterAST,
    operatorsBeforeAST,
    listOperatorsASTCheck,

    pemdasTree,

    sexprToAst,
) where

import Types
import Control.Exception
-- import Debug.Trace

-- INFO: Convert SExpr to AST
splitAtValue :: Eq a => a -> [a] -> Maybe ([a], a, [a])
splitAtValue _ [] = Nothing
splitAtValue val (x:xs)
    | val == x = Just ([], x, xs)
    | otherwise = case splitAtValue val xs of
                    Nothing -> Nothing
                    Just (before, v, after) -> Just (x:before, v, after)

splitAtLastValue :: Eq a => a -> [a] -> Maybe ([a], a, [a])
splitAtLastValue _ [] = Nothing
-- splitAtValue until we ge nothing
splitAtLastValue val (x:xs) = case splitAtValue val xs of
    Nothing -> Nothing
    Just (before, v, after) -> case splitAtLastValue val xs of
        Nothing -> Just (x:before, v, after)
        Just (before2, v2, after2) -> Just (x:before2, v2, after2)

getIfChain :: [Token] -> ([Token], [Token])
getIfChain [] = ([], [])
getIfChain (ElseIfToken : cond : expr : xs) = do
    let (ifChain, rest) = getIfChain xs
    (ElseIfToken : cond : expr : ifChain, rest)
getIfChain (ElseToken : expr : xs) = do
    let (ifChain, rest) = getIfChain xs
    (ElseToken : expr : ifChain, rest)
getIfChain xs = do
    ([], xs)

binaryOperatorsAST :: Token -> (AST -> AST -> AST) -> [Token] -> AST
binaryOperatorsAST token ast xs = do
    let (before, _, after) = case splitAtValue token xs of
            Nothing -> ([], token, [])
            Just (b, _, a) -> (b, token, a)
    ast (sexprToAst before) (sexprToAst after)

operatorsAfterAST :: Token -> (AST -> AST) -> [Token] -> AST
operatorsAfterAST token ast xs = do
    let (_, _, after) = case splitAtValue token xs of
            Nothing -> ([], token, [])
            Just (b, _, a) -> (b, token, a)
    ast (sexprToAst after)

operatorsBeforeAST :: Token -> (AST -> AST) -> [Token] -> AST
operatorsBeforeAST token ast xs = do
    let (before, _, _) = case splitAtValue token xs of
            Nothing -> ([], token, [])
            Just (b, _, a) -> (b, token, a)
    ast (sexprToAst before)

listOperatorsASTCheck :: [Token] -> [Token] -> Bool
listOperatorsASTCheck [] _ = True
listOperatorsASTCheck (token : tokens) xs = case splitAtValue token xs of
            Nothing -> False
            Just (_, _, _) -> listOperatorsASTCheck tokens xs

pemdasTreeAction :: Token -> (AST -> AST -> AST) -> [Token] -> AST
pemdasTreeAction token ast xs = do
    let (before, _, after) = case splitAtLastValue token xs of
            Nothing -> ([], token, [])
            Just (b, _, a) -> (b, token, a)
    ast (pemdasTree before) (pemdasTree after)

pemdasTreeAction2 :: Token -> Token -> (AST -> AST -> AST) -> (AST -> AST -> AST) -> [Token] -> AST
pemdasTreeAction2 token1 token2 ast1 ast2 xs = do
    let (before, _, after) = case splitAtLastValue token1 xs of
            Nothing -> ([], token1, [])
            Just (b, _, a) -> (b, token1, a)
    let (before2, _, after2) = case splitAtLastValue token2 xs of
            Nothing -> ([], token2, [])
            Just (b, _, a) -> (b, token2, a)
    if length before > length before2 then do
        ast1 (pemdasTree before) (pemdasTree after)
    else do
        ast2 (pemdasTree before2) (pemdasTree after2)

pemdasTree :: [Token] -> AST
pemdasTree [] = DeadLeafAST
-- ! Plus and Minus token
pemdasTree x | listOperatorsASTCheck [PlusToken, MinusToken] x = pemdasTreeAction2 PlusToken MinusToken PlusAST MinusAST x
-- ! Plus token
pemdasTree x | listOperatorsASTCheck [PlusToken] x = pemdasTreeAction PlusToken PlusAST x
-- ! Minus token
pemdasTree x | listOperatorsASTCheck [MinusToken] x = pemdasTreeAction MinusToken MinusAST x

-- ! Modulo, Divide and Times token
pemdasTree x | listOperatorsASTCheck [ModuloToken, DivideToken, TimesToken] x = do
    let (beforeModulo, _, _) = case splitAtLastValue ModuloToken x of
            Nothing -> ([], ModuloToken, [])
            Just (b, _, a) -> (b, ModuloToken, a)
    let (beforeDivide, _, _) = case splitAtLastValue DivideToken x of
            Nothing -> ([], DivideToken, [])
            Just (b, _, a) -> (b, DivideToken, a)
    let (beforeTimes, _, _) = case splitAtLastValue TimesToken x of
            Nothing -> ([], TimesToken, [])
            Just (b, _, a) -> (b, TimesToken, a)
    -- % * /
    -- * % /
    if (length beforeDivide > length beforeModulo) && (length beforeDivide > length beforeTimes) then do
        pemdasTreeAction DivideToken DivideAST x
    -- % / *
    -- / % *
    else if (length beforeTimes > length beforeDivide) && (length beforeTimes > length beforeModulo) then do
        pemdasTreeAction TimesToken TimesAST x
    -- / * %
    -- * / %
    -- else if (length beforeModulo > length beforeDivide) && (length beforeModulo > length beforeTimes) then do
    else do
        pemdasTreeAction ModuloToken ModuloAST x

-- ! Modulo and Divide token
pemdasTree x | listOperatorsASTCheck [ModuloToken, DivideToken] x = pemdasTreeAction2 ModuloToken DivideToken ModuloAST DivideAST x
-- ! Modulo and Times token
pemdasTree x | listOperatorsASTCheck [ModuloToken, TimesToken] x = pemdasTreeAction2 ModuloToken TimesToken ModuloAST TimesAST x
-- ! Divide and Times token
pemdasTree x | listOperatorsASTCheck [DivideToken, TimesToken] x = pemdasTreeAction2 DivideToken TimesToken DivideAST TimesAST x
-- ! Modulo token
pemdasTree x | listOperatorsASTCheck [ModuloToken] x = pemdasTreeAction ModuloToken ModuloAST x
-- ! Divide token
pemdasTree x | listOperatorsASTCheck [DivideToken] x = pemdasTreeAction DivideToken DivideAST x
-- ! Times token
pemdasTree x | listOperatorsASTCheck [TimesToken] x = pemdasTreeAction TimesToken TimesAST x

pemdasTree x = sexprToAst x

sexprToAst :: [Token] -> AST
sexprToAst [] = DeadLeafAST
-- ! Comma token
sexprToAst x | case splitAtValue CommaToken x of
            Nothing -> False
            Just (_, _, _) -> True = do
    let (before, _, after) = case splitAtValue CommaToken x of
            Nothing -> ([], CommaToken, [])
            Just (b, _, a) -> (b, CommaToken, a)
    -- case length of after without the CommaToken
    case length $ filter (/= CommaToken) after of
        -- if there is no more element after the CommaToken
        0 -> AST [sexprToAst before]
        -- add a CommaToken at the end of the list
        -- so after can be wrapped in an AST[] even if it's the last element
        _ -> AST [sexprToAst before] <> sexprToAst (after ++ [CommaToken])
-- ! While token
sexprToAst (WhileToken : cond : expr : xs) = do
    let cond2 = case cond of
            ListToken x -> x
            _ -> [cond]
    let expr2 = case expr of
            ListToken x -> x
            _ -> [expr]
    AST [WhileAST (sexprToAst cond2) (sexprToAst expr2)] <> sexprToAst xs
-- ! For token
sexprToAst (ForToken : ici : expr : xs) = do
    let ici2 = case ici of
            ListToken x -> x
            _ -> [ici]
    let expr2 = case expr of
            ListToken x -> x
            _ -> [expr]
    let (initer, _, rest) = case splitAtValue LineSeparator ici2 of
            Nothing -> ([], LineSeparator, [])
            Just (b, _, a) -> (b, LineSeparator, a)
    let (cond, _, rest2) = case splitAtValue LineSeparator rest of
            Nothing -> ([], LineSeparator, [])
            Just (b, _, a) -> (b, LineSeparator, a)
    let (incr, _, _) = case splitAtValue LineSeparator (rest2 ++ [LineSeparator]) of
            Nothing -> ([], LineSeparator, [])
            Just (b, _, a) -> (b, LineSeparator, a)
    AST [ForAST (sexprToAst initer) (sexprToAst cond) (sexprToAst incr) (sexprToAst expr2)] <> sexprToAst xs
-- ! Fun type token
sexprToAst (FunTypeToken : xs) = do
    FunTypeAST (sexprToAst xs)
-- ! Fun token
sexprToAst (FunToken : name : returnType : args : body : xs) = do
    let returnType2 = case returnType of
            ListToken x -> x
            _ -> [returnType]
    let args2 = case args of
            ListToken x -> x
            _ -> [args]
    let body2 = case body of
            ListToken x -> x
            _ -> [body]
    AST [FunAST (show name) (sexprToAst returnType2) (sexprToAst args2) (sexprToAst body2)] <> sexprToAst xs
-- ! If token
sexprToAst (IfToken : cond : expr : ElseIfToken : xs) = do
    let cond2 = case cond of
            ListToken x -> x
            _ -> [cond]
    let expr2 = case expr of
            ListToken x -> x
            _ -> [expr]
    let (ifChain, rest) = getIfChain (ElseIfToken : xs)
    AST [IfAST (sexprToAst cond2) (sexprToAst expr2) (sexprToAst ifChain)] <> sexprToAst rest
sexprToAst (IfToken : cond : expr : ElseToken : xs) = do
    let cond2 = case cond of
            ListToken x -> x
            _ -> [cond]
    let expr2 = case expr of
            ListToken x -> x
            _ -> [expr]
    let (ifChain, rest) = getIfChain (ElseToken : xs)
    AST [IfAST (sexprToAst cond2) (sexprToAst expr2) (sexprToAst ifChain)] <> sexprToAst rest
sexprToAst (IfToken : cond : expr : xs) = do
    let cond2 = case cond of
            ListToken x -> x
            _ -> [cond]
    let expr12 = case expr of
            ListToken x -> x
            _ -> [expr]
    AST [IfAST (sexprToAst cond2) (sexprToAst expr12) DeadLeafAST] <> sexprToAst xs
-- ! Else if token
sexprToAst (ElseIfToken : cond : expr : ElseIfToken : xs) = do
    let cond2 = case cond of
            ListToken x -> x
            _ -> [cond]
    let expr2 = case expr of
            ListToken x -> x
            _ -> [expr]
    let (ifChain, rest) = getIfChain (ElseIfToken : xs)
    AST [ElseIfAST (sexprToAst cond2) (sexprToAst expr2) (sexprToAst ifChain)] <> sexprToAst rest
sexprToAst (ElseIfToken : cond : expr : ElseToken : xs) = do
    let cond2 = case cond of
            ListToken x -> x
            _ -> [cond]
    let expr2 = case expr of
            ListToken x -> x
            _ -> [expr]
    let (ifChain, rest) = getIfChain (ElseToken : xs)
    AST [ElseIfAST (sexprToAst cond2) (sexprToAst expr2) (sexprToAst ifChain)] <> sexprToAst rest
sexprToAst (ElseIfToken : cond : expr : xs) = do
    let cond2 = case cond of
            ListToken x -> x
            _ -> [cond]
    let expr12 = case expr of
            ListToken x -> x
            _ -> [expr]
    AST [ElseIfAST (sexprToAst cond2) (sexprToAst expr12) DeadLeafAST] <> sexprToAst xs
-- ! Else token
sexprToAst (ElseToken : expr : xs) = do
    let expr2 = case expr of
            ListToken x -> x
            _ -> [expr]
    AST [ElseAST (sexprToAst expr2)] <> sexprToAst xs
-- ! Define token
sexprToAst (DefineToken : name : expr : xs) = do
    -- if type of name isn't SymbolToken then throw error
    if case name of
        SymbolToken _ -> False
        _ -> True then do
        throw $ (ParserError "Error: Invalid type for name in define")
    else do
        let expr2 = case expr of
                ListToken x -> x
                _ -> [expr]
        AST [DefineAST (show name) (sexprToAst expr2)] <> sexprToAst xs
-- ! Line separator token
sexprToAst x | case splitAtValue LineSeparator x of
            Nothing -> False
            Just (_, _, _) -> True = do
    let (before, _, after) = case splitAtValue LineSeparator x of
            Nothing -> ([], LineSeparator, [])
            Just (b, _, a) -> (b, LineSeparator, a)
    AST [sexprToAst before] <> sexprToAst after
-- ! Return token
sexprToAst (ReturnToken : expr : _) = do
    let expr2 = case expr of
            ListToken x -> x
            _ -> [expr]
    ReturnAST (sexprToAst expr2)
-- ! Symbol Tree
sexprToAst x | listOperatorsASTCheck [AssignToken] x =            binaryOperatorsAST AssignToken AssignAST x
sexprToAst x | listOperatorsASTCheck [PlusEqualToken] x =         binaryOperatorsAST PlusEqualToken PlusEqualAST x
sexprToAst x | listOperatorsASTCheck [MinusEqualToken] x =        binaryOperatorsAST MinusEqualToken MinusEqualAST x
sexprToAst x | listOperatorsASTCheck [TimesEqualToken] x =        binaryOperatorsAST TimesEqualToken TimesEqualAST x
sexprToAst x | listOperatorsASTCheck [DivideEqualToken] x =       binaryOperatorsAST DivideEqualToken DivideEqualAST x
sexprToAst x | listOperatorsASTCheck [ModuloEqualToken] x =       binaryOperatorsAST ModuloEqualToken ModuloEqualAST x
sexprToAst x | listOperatorsASTCheck [MinusToken] x =             pemdasTree x
sexprToAst x | listOperatorsASTCheck [PlusToken] x =              pemdasTree x
sexprToAst x | listOperatorsASTCheck [ModuloToken] x =            pemdasTree x
sexprToAst x | listOperatorsASTCheck [DivideToken] x =            pemdasTree x
sexprToAst x | listOperatorsASTCheck [TimesToken] x =             pemdasTree x
sexprToAst x | listOperatorsASTCheck [AndToken] x =               binaryOperatorsAST AndToken AndAST x
sexprToAst x | listOperatorsASTCheck [OrToken] x =                binaryOperatorsAST OrToken OrAST x
sexprToAst x | listOperatorsASTCheck [EqualToken] x =             binaryOperatorsAST EqualToken EqualAST x
sexprToAst x | listOperatorsASTCheck [NotEqualToken] x =          binaryOperatorsAST NotEqualToken NotEqualAST x
sexprToAst x | listOperatorsASTCheck [LessThanToken] x =          binaryOperatorsAST LessThanToken LessThanAST x
sexprToAst x | listOperatorsASTCheck [LessThanEqualToken] x =     binaryOperatorsAST LessThanEqualToken LessThanEqualAST x
sexprToAst x | listOperatorsASTCheck [GreaterThanToken] x =       binaryOperatorsAST GreaterThanToken GreaterThanAST x
sexprToAst x | listOperatorsASTCheck [GreaterThanEqualToken] x =  binaryOperatorsAST GreaterThanEqualToken GreaterThanEqualAST x

sexprToAst x | listOperatorsASTCheck [IncrementToken] x =         operatorsBeforeAST IncrementToken IncrementAST x
sexprToAst x | listOperatorsASTCheck [DecrementToken] x =         operatorsBeforeAST DecrementToken DecrementAST x
sexprToAst x | listOperatorsASTCheck [NotToken] x =               operatorsAfterAST NotToken NotAST x
-- ! Types token
sexprToAst (IntTypeToken : xs) = do
    AST [IntTypeAST] <> sexprToAst xs
sexprToAst (FloatTypeToken : xs) = do
    AST [FloatTypeAST] <> sexprToAst xs
sexprToAst (CharTypeToken : xs) = do
    AST [CharTypeAST] <> sexprToAst xs
sexprToAst (StringTypeToken : xs) = do
    AST [StringTypeAST] <> sexprToAst xs
-- ! Int token
sexprToAst (IntToken x : xs) = do
    AST [IntAST x] <> sexprToAst xs
-- ! Float token
sexprToAst (FloatToken x : xs) = do
    AST [FloatAST x] <> sexprToAst xs
-- ! Symbol token
sexprToAst (SymbolToken x : xs) = do
    AST [SymbolAST x] <> sexprToAst xs
-- ! String token
sexprToAst (StringToken x : xs) = do
    AST [StringAST x] <> sexprToAst xs
-- ! Char token
sexprToAst (CharToken x : xs) = do
    AST [CharAST x] <> sexprToAst xs
-- ! List token
sexprToAst (ListToken (x : xs) : ys) = do
    AST [sexprToAst (x : xs)] <> sexprToAst ys
-- ! Other token
sexprToAst (_ : xs) = do
    sexprToAst xs
