module Parser (
    parser,

    parseKeyword,
    parseIntToken,
    parseSymbolToken,
    parseStringToken,
    parseCharToken,

    parseToken,

    parseLine,
    mergeSymbols,
    parseFile,

    getSubList,
    tokenListToSexpr,

    splitAtValue,
    splitAtLastValue,
) where

import ParserModule
import System.Exit
import System.IO
import Types
import Control.Applicative
import Data.Char (chr)
import Control.Exception
-- import Debug.Trace

import System.FilePath (normalise)
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory)

getAbsolutePath :: FilePath -> IO FilePath
getAbsolutePath relativePath = do
    currentDir <- getCurrentDirectory
    return $ normalise (currentDir </> relativePath)

-- INFO: Token Parser
parseKeyword :: String -> Token -> Parser Token
parseKeyword str token = Parser f
    where
        f x = case runParser (parseString str) x of
            Just (_, xs) -> Just (token, xs)
            Nothing -> Nothing

parseFloatToken :: Parser Token
parseFloatToken = Parser f
    where
        f x = do
            num <- runParser parseInt x
            dot <- runParser (parseChar '.') (snd num)
            num2 <- runParser parseInt (snd dot)
            Just (FloatToken (fromIntegral (fst num) + (fromIntegral (fst num2) / (10 ^ (length (show (fst num2)))))), snd num2)

parseIntToken :: Parser Token
parseIntToken = Parser f
    where
        f x = case runParser parseInt x of
            Just (y, ys) -> Just (IntToken y, ys)
            Nothing -> Nothing

parseSymbolToken :: Parser Token
parseSymbolToken = Parser f
    where
        -- parse absolutely any char in UTF-16
        f x = case runParser (parseAnyChar [chr i | i <- [0x0000 .. 0x10FFFF]]) x of
            Just (y, ys) -> Just (SymbolToken [y], ys)
            Nothing -> Nothing

parseStringToken :: Parser Token
parseStringToken = Parser f
    where
        f x = case runParser (parseChar '\"') x of
            -- parse all chars in UTF-16 except "
            Just (_, ys) -> case runParser (parseMany (parseAnyChar [chr i | i <- [0x0000 .. 0x10FFFF], chr i /= '\"'])) ys of
                Just (z, zs) -> case runParser (parseChar '\"') zs of
                    Just (_, ws) -> Just (StringToken z, ws)
                    Nothing -> throw (ParserError "Missing \" token")
                Nothing -> Nothing
            Nothing -> Nothing

parseCharToken :: Parser Token
parseCharToken = Parser f
    where
        f x = case runParser (parseChar '\'') x of
            -- parse all chars in UTF-16 except '
            Just (_, ys) -> case runParser (parseAnyChar [chr i | i <- [0x0000 .. 0x10FFFF], chr i /= '\'']) ys of
                Just (z, zs) -> case runParser (parseChar '\'') zs of
                    Just (_, ws) -> Just (CharToken z, ws)
                    Nothing -> throw (ParserError "Missing ' token")
                Nothing -> Nothing
            Nothing -> Nothing

parseToken :: Parser Token
parseToken =
    -- KeyWords
    (parseKeyword "#define" DefineToken)
    <|> (parseKeyword "#include" IncludeToken)
    <|> (parseKeyword "if" IfToken)
    <|> (parseKeyword "else" ElseToken)
    <|> (parseKeyword "for" ForToken)
    <|> (parseKeyword "while" WhileToken)
    <|> (parseKeyword "fun" FunToken)
    <|> (parseKeyword ":" FunTypeToken)
    -- Types
    <|> (parseKeyword "int" IntTypeToken)
    <|> (parseKeyword "float" FloatTypeToken)
    <|> (parseKeyword "char" CharTypeToken)
    <|> (parseKeyword "string" StringTypeToken)
    -- Comments
    <|> (parseKeyword "/*" CommentStart)
    <|> (parseKeyword "*/" CommentEnd)
    <|> (parseKeyword "//" InlineCommentStart)
    -- Separator
    <|> (parseKeyword "," CommaToken)
    -- Line separator
    <|> (parseKeyword ";" LineSeparator)
    -- Spacer
    <|> (parseKeyword " " SpaceToken)
    <|> (parseKeyword "\t" SpaceToken)
    -- Operators
    <|> (parseKeyword "++" IncrementToken)
    <|> (parseKeyword "--" DecrementToken)
    <|> (parseKeyword "+=" PlusEqualToken)
    <|> (parseKeyword "-=" MinusEqualToken)
    <|> (parseKeyword "*=" TimesEqualToken)
    <|> (parseKeyword "/=" DivideEqualToken)
    <|> (parseKeyword "%=" ModuloEqualToken)
    <|> (parseKeyword "&&" AndToken)
    <|> (parseKeyword "||" OrToken)
    <|> (parseKeyword "==" EqualToken)
    <|> (parseKeyword "!=" NotEqualToken)
    <|> (parseKeyword "<=" LessThanEqualToken)
    <|> (parseKeyword ">=" GreaterThanEqualToken)
    <|> (parseKeyword "+" PlusToken)
    <|> (parseKeyword "-" MinusToken)
    <|> (parseKeyword "*" TimesToken)
    <|> (parseKeyword "/" DivideToken)
    <|> (parseKeyword "%" ModuloToken)
    <|> (parseKeyword "<" LessThanToken)
    <|> (parseKeyword ">" GreaterThanToken)
    <|> (parseKeyword "!" NotToken)
    <|> (parseKeyword "=" AssignToken)
    -- List
    <|> (parseKeyword "(" OpenParenthesis)
    <|> (parseKeyword ")" CloseParenthesis)
    <|> (parseKeyword "[" OpenBracket)
    <|> (parseKeyword "]" CloseBracket)
    <|> (parseKeyword "{" OpenBraces)
    <|> (parseKeyword "}" CloseBraces)
    -- Quotes
    <|> (parseStringToken)
    <|> (parseCharToken)
    -- Numbers
    <|> (parseFloatToken)
    <|> (parseIntToken)
    -- Symbols (others)
    <|> (parseSymbolToken)
    -- Empty str
    <|> empty

-- INFO: Create token list
parseLine :: String -> Int -> String -> IO ([Token])
parseLine str lineNumber filename =
    catch (
        -- for each word in the string generate the tokens
        case runParser (parseMany parseToken) str of
            Just (x, _) -> do
                return (x)
            Nothing -> do
                throw (ParserError ("Invalid syntax on file: " ++ show filename ++ " at line " ++ show lineNumber ++ ": " ++ str))
        ) handler
    where
        handler :: ParserError -> IO ([Token])
        handler e = do
            throw (ParserError ("Invalid syntax on file: " ++ show filename ++ " at line " ++ show lineNumber ++ ":\n" ++ str ++ "\n" ++ show e))

includeFile :: String -> [String] -> IO ([Token])
includeFile str filenames = do
    contents <- readFile str
    file <- return $ File (lines contents)
    parseFile file 1 filenames

mergeSymbols :: [Token] -> [String] -> IO [Token]
mergeSymbols [] _ = return []
-- Once we found a InlineCommentStart we ignore all the rest of the line
mergeSymbols (InlineCommentStart : _) _ = return []
-- include a file
mergeSymbols (IncludeToken : xs) filenames | head (filter (/= SpaceToken) xs) == StringToken str = do
    filename <- getAbsolutePath str
    rest <- mergeSymbols (tail (dropWhile (/= StringToken str) xs)) (filename : filenames)
    if filename `elem` filenames then do
        -- throw (ParserError ("Error: Recursive include of file " ++ str))
        return (rest)
    else do
        includedFile <- includeFile str (filename : filenames)
        return (includedFile ++ rest)
    where
        str = case head (filter (/= SpaceToken) xs) of
            StringToken x -> x
            _ -> throw (ParserError "Error: Invalid type for include")
-- merge all consecutive symbols (ex: b o n j o u r  -> bonjour)
mergeSymbols (SymbolToken x : SymbolToken y : xs) filenames = mergeSymbols (SymbolToken (x ++ y) : xs) filenames
mergeSymbols (SymbolToken x : IntTypeToken : xs) filenames = mergeSymbols (SymbolToken (x ++ "int") : xs) filenames
mergeSymbols (SymbolToken x : FloatTypeToken : xs) filenames = mergeSymbols (SymbolToken (x ++ "float") : xs) filenames
mergeSymbols (SymbolToken x : CharTypeToken : xs) filenames = mergeSymbols (SymbolToken (x ++ "char") : xs) filenames
mergeSymbols (SymbolToken x : StringTypeToken : xs) filenames = mergeSymbols (SymbolToken (x ++ "string") : xs) filenames
mergeSymbols (SymbolToken x : IfToken : xs) filenames = mergeSymbols (SymbolToken (x ++ "if") : xs) filenames
mergeSymbols (SymbolToken x : ElseToken : xs) filenames = mergeSymbols (SymbolToken (x ++ "else") : xs) filenames
mergeSymbols (SymbolToken x : FunToken : xs) filenames = mergeSymbols (SymbolToken (x ++ "fun") : xs) filenames
mergeSymbols (SymbolToken x : ForToken : xs) filenames = mergeSymbols (SymbolToken (x ++ "for") : xs) filenames
mergeSymbols (SymbolToken x : WhileToken : xs) filenames = mergeSymbols (SymbolToken (x ++ "while") : xs) filenames
mergeSymbols (SymbolToken x : IntToken y : xs) filenames = mergeSymbols (SymbolToken (x ++ show y) : xs) filenames

mergeSymbols (IntTypeToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("int" ++ x) : xs) filenames
mergeSymbols (FloatTypeToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("float" ++ x) : xs) filenames
mergeSymbols (CharTypeToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("char" ++ x) : xs) filenames
mergeSymbols (StringTypeToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("string" ++ x) : xs) filenames
mergeSymbols (IfToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("if" ++ x) : xs) filenames
mergeSymbols (ElseToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("else" ++ x) : xs) filenames
mergeSymbols (FunToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("fun" ++ x) : xs) filenames
mergeSymbols (ForToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("for" ++ x) : xs) filenames
mergeSymbols (WhileToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("while" ++ x) : xs) filenames
mergeSymbols (IntToken x : SymbolToken y : xs) filenames = mergeSymbols (SymbolToken (show x ++ y) : xs) filenames

-- merge else if to elif
mergeSymbols (ElseToken : xs) filenames | (head (filter (/= SpaceToken) xs)) == IfToken = mergeSymbols (ElseIfToken : (tail (dropWhile (/= IfToken) xs))) filenames
-- merge negative numbers (ex: - 123 -> -123)
mergeSymbols (MinusToken : IntToken x : xs) filenames = mergeSymbols (IntToken (-x) : xs) filenames
mergeSymbols (MinusToken : FloatToken x : xs) filenames = mergeSymbols (FloatToken (-x) : xs) filenames
-- Delete all spaces
mergeSymbols (SpaceToken : xs) filenames = mergeSymbols xs filenames
-- Concat all LineSeparator
mergeSymbols (LineSeparator : LineSeparator : xs) filenames = mergeSymbols (LineSeparator : xs) filenames
-- No merge needed
mergeSymbols (x:xs) filenames = do
    rest <- mergeSymbols xs filenames
    return (x : rest)

parseFile :: File -> Int -> [String] -> IO ([Token])
parseFile (File []) _ _ = return ([])
parseFile (File (x:xs)) lineNumber filenames = do
    parsedLine <- parseLine x lineNumber (head filenames)
    currentLine <- mergeSymbols parsedLine filenames
    rest <- parseFile (File xs) (lineNumber + 1) filenames
    return (currentLine ++ rest)

-- INFO: Convert token list to SExpr
getSubList :: Token -> Token -> [Token] -> ([Token], [Token])
getSubList _ _ [] = ([], [])
-- Comment case
getSubList CommentStart CommentEnd (x:xs) | x == CommentEnd = ([], xs)
getSubList CommentStart CommentEnd (_:xs) = getSubList CommentStart CommentEnd xs
-- All case
getSubList _ close (x:xs) | x == close = ([], xs)
getSubList open close (x:xs) | x == open = do
    let (subList, rest) = getSubList open close xs
    let (subList2, rest2) = getSubList open close rest
    (open : subList ++ close : subList2, rest2)
getSubList open close (x:xs) = do
    let (subList, rest) = getSubList open close xs
    (x : subList, rest)

tokenListToSexpr :: [Token] -> [Token]
tokenListToSexpr [] = []
-- all between comment is ignored
tokenListToSexpr (CommentStart : xs) = do
    let (_, rest) = getSubList CommentStart CommentEnd xs
    tokenListToSexpr rest
-- Create a sub list for function type
tokenListToSexpr (FunTypeToken : xs) = do
    ListToken [FunTypeToken, head (tokenListToSexpr xs)] : (tail (tokenListToSexpr xs))
-- all between parenthesis is a sub list of tokens
tokenListToSexpr (OpenParenthesis : xs) = do
    let (subList, rest) = getSubList OpenParenthesis CloseParenthesis xs
    ListToken (tokenListToSexpr subList) : tokenListToSexpr rest
-- all between bracket is a sub list of tokens
tokenListToSexpr (OpenBracket : xs) = do
    let (subList, rest) = getSubList OpenBracket CloseBracket xs
    ListToken (tokenListToSexpr subList) : tokenListToSexpr rest
-- all between braces is a sub list of tokens
tokenListToSexpr (OpenBraces : xs) = do
    let (subList, rest) = getSubList OpenBraces CloseBraces xs
    ListToken (tokenListToSexpr subList) : tokenListToSexpr rest
-- all other tokens are converted to SExpr
tokenListToSexpr (x:xs) = x : tokenListToSexpr xs

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
    let (before, _, after) = case splitAtValue token xs of
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
    -- % / *
    -- * % /
    if length beforeDivide > length beforeModulo then do
        pemdasTreeAction2 ModuloToken TimesToken ModuloAST TimesAST x
    -- / * %
    -- / % *
    else if length beforeTimes > length beforeDivide then do
        pemdasTreeAction2 ModuloToken DivideToken ModuloAST DivideAST x
    -- * / %
    else do
        pemdasTreeAction2 DivideToken TimesToken DivideAST TimesAST x

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

-- INFO: Main function
checkSyntax :: [Token] -> IO ()
checkSyntax xs = do
    let nbOpenParenthesis = length $ filter (== OpenParenthesis) xs
    let nbCloseParenthesis = length $ filter (== CloseParenthesis) xs
    let nbOpenBracket = length $ filter (== OpenBracket) xs
    let nbCloseBracket = length $ filter (== CloseBracket) xs
    let nbOpenBraces = length $ filter (== OpenBraces) xs
    let nbCloseBraces = length $ filter (== CloseBraces) xs
    let nbCommentStart = length $ filter (== CommentStart) xs
    let nbCommentEnd = length $ filter (== CommentEnd) xs
    if nbOpenParenthesis < nbCloseParenthesis then do
        throw $ (ParserError "Error: Missing ( token")
    else if nbOpenParenthesis > nbCloseParenthesis then do
        throw $ (ParserError "Error: Missing ) token")
    else if nbOpenBracket < nbCloseBracket then do
        throw $ (ParserError "Error: Missing [ token")
    else if nbOpenBracket > nbCloseBracket then do
        throw $ (ParserError "Error: Missing ] token")
    else if nbOpenBraces < nbCloseBraces then do
        throw $ (ParserError "Error: Missing { token")
    else if nbOpenBraces > nbCloseBraces then do
        throw $ (ParserError "Error: Missing } token")
    else if nbCommentStart /= nbCommentEnd then do
        throw $ (ParserError "Error: Missing */ token")
    else if IncludeToken `elem` xs then do
        throw $ (ParserError "Error: #include token must be followed by a string for the file to include")
    else do
        return ()

parser :: File -> String -> IO (AST)
parser file filename = do
    catch (
        do
        putStrLn "------------------------------------"
        putStrLn $ show file
        putStrLn "------------------------------------"
        absoluteFilename <- getAbsolutePath filename
        tokenList <- parseFile file 1 [absoluteFilename]
        checkSyntax tokenList
        putStrLn $ show $ tokenList
        putStrLn "------------------------------------"
        putStrLn $ show $ tokenListToSexpr tokenList
        putStrLn "------------------------------------"
        let sexpr = tokenListToSexpr tokenList
        putStrLn $ printAST $ sexprToAst sexpr
        putStrLn "------------------------------------"
        let ast = sexprToAst sexpr
        return (ast)
        ) handler
    where
        handler :: ParserError -> IO (AST)
        handler e = do
            hPutStrLn stderr $ show e
            exitWith (ExitFailure 84)
