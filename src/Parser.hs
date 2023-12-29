module Parser (
    parser,
) where

import ParserModule
import System.Exit
import System.IO
import Types
import Control.Applicative
import Data.Char (chr)
import Control.Exception
-- import Debug.Trace

-- INFO: Token Parser
parseKeyword :: String -> Token -> Parser Token
parseKeyword str token = Parser f
    where
        f x = case runParser (parseString str) x of
            Just (_, xs) -> Just (token, xs)
            Nothing -> Nothing

parseIntToken :: Parser Token
parseIntToken = Parser f
    where
        f x = case runParser parseInt x of
            Just (y, ys) -> Just (IntToken y, ys)
            Nothing -> Nothing

parseSymbolToken :: Parser Token
parseSymbolToken = Parser f
    where
        -- parse absolutely any char
        f x = case runParser (parseAnyChar ['\0' .. '\255']) x of
            Just (y, ys) -> Just (SymbolToken [y], ys)
            Nothing -> Nothing

parseStringToken :: Parser Token
parseStringToken = Parser f
    where
        f x = case runParser (parseChar '\"') x of
            -- parse all chars except "
            Just (_, ys) -> case runParser (parseMany (parseAnyChar [chr i | i <- [0 .. 255], chr i /= '\"'])) ys of
                Just (z, zs) -> case runParser (parseChar '\"') zs of
                    Just (_, ws) -> Just (StringToken z, ws)
                    Nothing -> throw (ParserError "Missing \" token")
                Nothing -> Nothing
            Nothing -> Nothing

parseCharToken :: Parser Token
parseCharToken = Parser f
    where
        f x = case runParser (parseChar '\'') x of
            -- parse all chars except '
            Just (_, ys) -> case runParser (parseAnyChar [chr i | i <- [0 .. 255], chr i /= '\'']) ys of
                Just (z, zs) -> case runParser (parseChar '\'') zs of
                    Just (_, ws) -> Just (CharToken z, ws)
                    Nothing -> throw (ParserError "Missing ' token")
                Nothing -> Nothing
            Nothing -> Nothing

parseToken :: Parser Token
parseToken =
    -- KeyWords
    (parseKeyword "#define" DefineToken)
    <|> (parseKeyword "if" IfToken)
    <|> (parseKeyword "else" ElseToken)
    <|> (parseKeyword "lambda" LambdaToken)
    <|> (parseKeyword "fun" FunToken)
    <|> (parseKeyword ":" FunTypeToken)
    -- Types
    <|> (parseKeyword "int" IntTypeToken)
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
    <|> (parseIntToken)
    -- Symbols (others)
    <|> (parseSymbolToken)
    -- Empty str
    <|> empty

-- INFO: Create token list
parseLine :: String -> Int -> IO ([Token])
parseLine str lineNumber =
    catch (
        -- for each word in the string generate the tokens
        case runParser (parseMany parseToken) str of
            Just (x, _) -> do
                return (x)
            Nothing -> do
                throw (ParserError ("Invalid syntax at line " ++ show lineNumber ++ ": " ++ str))
        ) handler
    where
        handler :: ParserError -> IO ([Token])
        handler e = do
            throw (ParserError ("Invalid syntax at line " ++ show lineNumber ++ ":\n" ++ str ++ "\n" ++ show e))

mergeSymbols :: [Token] -> [Token]
mergeSymbols [] = []
-- Once we found a InlineCommentStart we ignore all the rest of the line
mergeSymbols (InlineCommentStart : _) = []
-- merge all consecutive symbols (ex: b o n j o u r  -> bonjour)
mergeSymbols (SymbolToken x : SymbolToken y : xs) = mergeSymbols (SymbolToken (x ++ y) : xs)
mergeSymbols (SymbolToken x : IntTypeToken : xs) = mergeSymbols (SymbolToken (x ++ "int") : xs)
mergeSymbols (SymbolToken x : CharTypeToken : xs) = mergeSymbols (SymbolToken (x ++ "char") : xs)
mergeSymbols (SymbolToken x : StringTypeToken : xs) = mergeSymbols (SymbolToken (x ++ "string") : xs)
mergeSymbols (SymbolToken x : IfToken : xs) = mergeSymbols (SymbolToken (x ++ "if") : xs)
mergeSymbols (SymbolToken x : ElseToken : xs) = mergeSymbols (SymbolToken (x ++ "else") : xs)
mergeSymbols (SymbolToken x : LambdaToken : xs) = mergeSymbols (SymbolToken (x ++ "lambda") : xs)
mergeSymbols (SymbolToken x : FunToken : xs) = mergeSymbols (SymbolToken (x ++ "fun") : xs)
-- merge all consecutive numbers (ex: 1 2 3 -> 123)
mergeSymbols (IntToken x : IntToken y : xs) = mergeSymbols (IntToken (x * 10 + y) : xs)
-- Delete all spaces
mergeSymbols (SpaceToken : xs) = mergeSymbols xs
-- Concat all LineSeparator
mergeSymbols (LineSeparator : LineSeparator : xs) = mergeSymbols (LineSeparator : xs)
-- No merge needed
mergeSymbols (x:xs) = x : mergeSymbols xs

parseFile :: File -> Int -> IO ([Token])
parseFile (File []) _ = return ([])
parseFile (File (x:xs)) lineNumber = do
    parsedLine <- parseLine x lineNumber
    rest <- parseFile (File xs) (lineNumber + 1)
    return ((mergeSymbols parsedLine) ++ rest)

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
    ListToken [FunTypeToken, head xs] : tokenListToSexpr (tail xs)
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
sexprToAst (IfToken : cond : expr : xs) = do
    let cond2 = case cond of
            ListToken x -> x
            _ -> [cond]
    let expr12 = case expr of
            ListToken x -> x
            _ -> [expr]
    AST [IfAST (sexprToAst cond2) (sexprToAst expr12)] <> sexprToAst xs
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
-- ! Lambda token
sexprToAst (LambdaToken : xs) = do
    let (args, body) = (head xs, tail xs)
    let args2 = case args of
            ListToken x -> x
            _ -> [args]
    LambdaAST (sexprToAst args2) (sexprToAst body)
-- ! Line separator token
sexprToAst x | case splitAtValue LineSeparator x of
            Nothing -> False
            Just (_, _, _) -> True = do
    let (before, _, after) = case splitAtValue LineSeparator x of
            Nothing -> ([], LineSeparator, [])
            Just (b, _, a) -> (b, LineSeparator, a)
    AST [sexprToAst before] <> sexprToAst after
-- ! Types token
sexprToAst (IntTypeToken : xs) = do
    AST [IntTypeAST] <> sexprToAst xs
sexprToAst (CharTypeToken : xs) = do
    AST [CharTypeAST] <> sexprToAst xs
sexprToAst (StringTypeToken : xs) = do
    AST [StringTypeAST] <> sexprToAst xs
-- ! Int token
sexprToAst (IntToken x : xs) = do
    AST [IntAST x] <> sexprToAst xs
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
    else do
        return ()

parser :: File -> IO (AST)
parser file = do
    catch (
        do
        putStrLn "------------------------------------"
        putStrLn $ show file
        putStrLn "------------------------------------"
        tokenList <- parseFile file 1
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
