module Parser (
    parser,

    Parser (..),
    parseChar,
    parseString,
    parseAnyChar,
    parseOr,
    parseAnd,
    parseAndWith,
    parseMany,
    parseSome,
    parseUInt,
    parseInt,
    parsePair,
    parseOrBoth,
    parseList,
) where

import System.Exit
import System.IO
import Types
import Control.Applicative
import Data.Char (chr)
import Control.Exception
-- import Debug.Trace

-- INFO: Parsing bootstrap
data Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap f (Parser g) = Parser h
        where
            h x = case g x of
                Just (y, ys) -> Just (f y, ys)
                Nothing -> Nothing

instance Applicative Parser where
    pure x = Parser f
        where
            f xs = Just (x, xs)
    (Parser f) <*> (Parser g) = Parser h
        where
            h x = case f x of
                Just (y, ys) -> case g ys of
                    Just (z, zs) -> Just (y z, zs)
                    Nothing -> Nothing
                Nothing -> Nothing

instance Alternative Parser where
    empty = Parser f
        where
            f _ = Nothing
    (Parser f) <|> (Parser g) = Parser h
        where
            h x = case f x of
                Just (y, ys) -> Just (y, ys)
                Nothing -> case g x of
                    Just (z, zs) -> Just (z, zs)
                    Nothing -> Nothing

-- ! NOT SURE OF THEIR USE
-- Operation associative binaire -> e.g: a * (b * c) = (a * b) * c
instance Semigroup a => Semigroup (Parser a) where
    (Parser f) <> (Parser g) = Parser h
        where
            h x = case f x of
                Just (y, ys) -> case g ys of
                    -- x y = x mappend y
                    Just (z, zs) -> Just (y <> z, zs)
                    -- x nothing = x
                    Nothing -> Just (y, ys)
                Nothing -> case g x of
                    -- nothing y = y
                    Just (z, zs) -> Just (z, zs)
                    -- nothing nothing = nothing
                    Nothing -> Nothing

-- Operation associative binaire avec element neutre -> e.g: a * (b * c) = (a * b) * c
-- element neutre (E) -> e.g: a * E = a, a + E = a
instance Monoid a => Monoid (Parser a) where
    mempty = Parser f
        where
            f _ = Nothing
    mappend = (<>)
-- ! NOT SURE OF THEIR USE

instance Monad Parser where
    return = pure
    (Parser f) >>= g = Parser h
        where
            h x = case f x of
                Just (y, ys) -> runParser (g y) ys
                Nothing -> Nothing

parseChar :: Char -> Parser Char
parseChar x = Parser f
    where
        f (y:ys) | x == y = Just (x, ys)
        f _ = Nothing

parseString :: String -> Parser String
parseString x = Parser f
    where
        -- using Monad (do notation)
        f xs | length x == 0 = Just ("", xs)
        f xs = do
            (_, ys) <- runParser (parseChar (head x)) xs
            (z, zs) <- runParser (parseString (tail x)) ys
            return (head x : z, zs)
        -- f xs = case length x of
        --     0 -> Just ("", xs)
        --     _ -> case runParser (parseChar (head x)) xs of
        --         Just (_, ys) -> case runParser (parseString (tail x)) ys of
        --             Just (z, zs) -> Just (head x : z, zs)
        --             Nothing -> Nothing
        --         Nothing -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar x = Parser f
    where
        f (y:ys) | y `elem` x = Just (y, ys)
        f _ = Nothing

parseOr :: Parser a -> Parser a -> Parser a
parseOr (Parser f) (Parser g) = Parser h
    where
        -- using Alternative instance
        h x = f x <|> g x

        -- h x = case f x of
        --     Just (y, ys) -> Just (y, ys)
        --     Nothing -> g x

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd (Parser f) (Parser g) = Parser h
    where
        -- using Monad (do notation)
        h x = do
            (y, ys) <- f x
            (z, zs) <- g ys
            return ((y, z), zs)
        -- h x = case f x of
        --     Just (y, ys) -> case g ys of
        --         Just (z, zs) -> Just ((y, z), zs)
        --         Nothing -> Nothing
        --     Nothing -> Nothing

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f (Parser g) (Parser h) = Parser i
    where
        -- using Monad (do notation)
        i x = do
            (y, ys) <- g x
            (z, zs) <- h ys
            return (f y z, zs)
        -- i x = case g x of
        --     Just (y, ys) -> case h ys of
        --         Just (z, zs) -> Just (f y z, zs)
        --         Nothing -> Nothing
        --     Nothing -> Nothing

parseMany :: Parser a -> Parser [a]
parseMany (Parser f) = Parser g
    where
        g x = case f x of
            Just (y, ys) -> case g ys of
                Just (z, zs) -> Just (y : z, zs)
                Nothing -> Just ([y], ys)
            Nothing -> Just ([], x)

parseSome :: Parser a -> Parser [a]
-- Using Functor and Applicative instance <$> and <*>
parseSome f = Parser g
    where
        g x = runParser (pure (:) <*> f <*> parseMany f) x
-- parseSome (Parser f) = Parser g
--     where
--         g x = case f x of
--             Just (y, ys) -> case g ys of
--                 Just (z, zs) -> Just (y : z, zs)
--                 Nothing -> Just ([y], ys)
--             Nothing -> Nothing

parseUInt :: Parser Int
parseUInt = Parser f
    where
        -- using the functor instance
        -- f x = runParser (fmap (\ y -> read y :: Int) (parseSome (parseAnyChar ['0' .. '9']))) x
        f x = runParser ((\ y -> read y :: Int) <$> (parseSome (parseAnyChar ['0' .. '9']))) x -- <$> is the infix version of fmap
    -- where
    --     f x = case runParser (parseSome (parseAnyChar ['0' .. '9'])) x of
    --         Just (y, ys) -> Just (read y :: Int, ys)
    --         Nothing -> Nothing

parseSign :: Parser Int
parseSign = Parser f
    where
        f ('-':xs) = Just (-1, xs)
        f ('+':xs) = Just (1, xs)
        f xs = Just (1, xs)

parseInt :: Parser Int
parseInt = Parser f
    where
        -- Using Functor and Applicative instance <$> and <*>
        f x = runParser (pure (\ y z -> y * z) <*> parseSign <*> parseUInt) x

        -- f ('-':xs) = do
        --     (x, ys) <- runParser parseUInt xs
        --     return (-x, ys)
        -- f ('+':xs) = do
        --     (x, ys) <- runParser parseUInt xs
        --     return (x, ys)
        -- f xs = runParser parseUInt xs

parsePair :: Parser a -> Parser (a, a) -- Dumb since the format is fixed
parsePair (Parser f) = Parser h
    where
        -- Using Monad (do notation)
        h x = do
            (_, as) <- runParser (parseChar '(') x
            (b, bs) <- f as
            (_, cs) <- runParser (parseChar ' ') bs
            (d, es) <- f cs
            (_, fs) <- runParser (parseChar ')') es
            return ((b, d), fs)
        -- h x = case runParser (parseChar '(') x of
        --     Just (_, as) -> case f as of
        --         Just (b, bs) -> case runParser (parseChar ' ') bs of
        --             Just (_, cs) -> case f cs of
        --                 Just (d, es) -> case runParser (parseChar ')') es of
        --                     Just (_, fs) -> Just ((b, d), fs)
        --                     Nothing -> Nothing
        --                 Nothing -> Nothing
        --             Nothing -> Nothing
        --         Nothing -> Nothing
        --     Nothing -> Nothing


-- parseList' :: Char -> Char -> Char -> Parser a -> Parser [a]
-- parseList' _ sep close (Parser f) = Parser h
--     where
--         h xs = case f xs of -- check parse
--             Just (b, bs) -> case runParser (parseChar sep) bs of -- if parse then check sep
--                 Just (_, cs) -> case h cs of -- if sep then check list
--                     Just (d, es) -> Just (b : d, es)
--                     Nothing -> case runParser (parseChar close) cs of -- if not list then check close
--                         Just (_, fs) -> Just ([b], fs)
--                         Nothing -> Nothing
--                 Nothing -> case runParser (parseChar close) bs of -- if not sep then check close
--                     Just (_, cs) -> Just ([b], cs)
--                     Nothing -> Nothing
--             Nothing -> case runParser (parseChar close) xs of -- if not parse then check close
--                 Just (_, bs) -> Just ([], bs)
--                 Nothing -> Nothing

-- parseList :: Char -> Char -> Char -> Parser a -> Parser [a]
-- parseList open sep close (Parser f) = Parser h
--     where
--         h x = case runParser (parseAnd (parseChar open) (parseList' open sep close (Parser f))) x of
--             Just ((_, y), ys) -> Just (y, ys)
--             Nothing -> Nothing

parseOrBoth :: Parser a -> Parser b -> Parser (Either a b)
parseOrBoth (Parser f) (Parser g) = Parser h
    where
        h x = case f x of
            Just (y, ys) -> Just (Left y, ys)
            Nothing -> case g x of
                Just (z, zs) -> Just (Right z, zs)
                Nothing -> Nothing

--           open        separator   close       ignore      token       result
parseList :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser [e]
parseList open sep close ignore token = Parser f
    where
        -- Using Monad (do notation)
        f x = do
            (_, xs) <- runParser open x
            (y, ys) <- runParser (parseMany (parseAndWith (\ _ y -> y) (parseMany (parseOrBoth sep ignore)) token)) xs
            (_, zs) <- runParser close ys
            return (y, zs)
        -- f x = case runParser open x of
        --     Just (_, xs) -> case runParser (parseMany (parseAndWith (\ _ y -> y) (parseMany (parseOrBoth sep ignore)) token)) xs of
        --         Just (y, ys) -> case runParser close ys of
        --             Just (_, zs) -> Just (y, zs)
        --             Nothing -> Nothing
        --         Nothing -> Nothing
        --     Nothing -> Nothing

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
                    Just (_, ws) -> Just (SymbolToken ("\"" ++ z ++ "\""), ws)
                    Nothing -> throw (ParserError "Missing \" token")
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
                -- hPutStrLn stderr $ "Invalid syntax at line " ++ show lineNumber ++ ": " ++ str
                -- exitWith (ExitFailure 84)
        ) handler
    where
        handler :: ParserError -> IO ([Token])
        handler e = do
            throw (ParserError ("Invalid syntax at line " ++ show lineNumber ++ ": " ++ show e))
            -- hPutStrLn stderr $ "Invalid syntax at line " ++ show lineNumber ++ ": " ++ show e
            -- exitWith (ExitFailure 84)

    -- case words str of
    --     [] -> do
    --         return ([])
    --     ("define":xs) -> do
    --         tmpDefine <- parseLine (" " ++ unwords xs) lineNumber
    --         return ([DefineToken] ++ tmpDefine)
    --     ("if":xs) -> do
    --         tmpIf <- parseLine (" " ++ unwords xs) lineNumber
    --         return ([IfToken] ++ tmpIf)
    --     ("lambda":xs) -> do
    --         tmpLambda <- parseLine (" " ++ unwords xs) lineNumber
    --         return ([LambdaToken] ++ tmpLambda)
    --     (_:_) -> do
    --         -- for each char in the string generate the tokens
    --         case str of
    --             [] -> do
    --                 return ([])
    --             (' ':ys) -> do
    --                 tmpSpace <- parseLine ys lineNumber
    --                 return ([SpaceToken] ++ tmpSpace)
    --             ('(':ys) -> do
    --                 tmpOpenParenthesis <- parseLine ys lineNumber
    --                 return ([OpenParenthesis] ++ tmpOpenParenthesis)
    --             (')':ys) -> do
    --                 tmpCloseParenthesis <- parseLine ys lineNumber
    --                 return ([CloseParenthesis] ++ tmpCloseParenthesis)
    --             ('\"':ys) -> do
    --                 let (quoteStr, rest) = span (/= '\"') ys
    --                 if null rest then do
    --                     hPutStrLn stderr $ "Error: Missing \" at line " ++ show lineNumber
    --                     exitWith (ExitFailure 84)
    --                 else do
    --                     tmpQuoteStr <- parseLine (tail rest) lineNumber
    --                     return ([SymbolToken quoteStr] ++ tmpQuoteStr)
    --             (y:ys) | y `elem` ['0'..'9'] -> do
    --                 tmpInt <- parseLine ys lineNumber
    --                 return ([IntToken (read [y])] ++ tmpInt)
    --             (y:ys) -> do
    --                 tmpSymbol <- parseLine ys lineNumber
    --                 return ([SymbolToken [y]] ++ tmpSymbol)

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
-- -- Trim all spaces
-- mergeSymbols (SpaceToken : SpaceToken : xs) = mergeSymbols (SpaceToken : xs)
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
    -- let (name, rest) = (head xs, tail xs)
    -- let (args, rest2) = getSubList OpenParenthesis CloseParenthesis rest
    -- let (body, rest3) = getSubList OpenBraces CloseBraces rest2
    AST [FunAST (show name) (sexprToAst returnType2) (sexprToAst args2) (sexprToAst body2)] <> sexprToAst xs
-- ! If token
sexprToAst (IfToken : cond : expr : xs) = do
    -- let (cond, expr1) = (head xs, head (tail xs))
    let cond2 = case cond of
            ListToken x -> x
            _ -> [cond]
    let expr12 = case expr of
            ListToken x -> x
            _ -> [expr]
    -- let expr22 = case expr2 of
    --         ListToken x -> x
    --         _ -> [expr2]
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
-- ! Int token
-- sexprToAst (IntToken x : ListToken y : _) = do
--     AST [IntAST x, sexprToAst y]
sexprToAst (IntToken x : xs) = do
    AST [IntAST x] <> sexprToAst xs
-- ! Symbol token
-- sexprToAst (SymbolToken x : ListToken y : _) = do
--     AST [SymbolAST x, sexprToAst y]
sexprToAst (SymbolToken x : xs) = do
    AST [SymbolAST x] <> sexprToAst xs
-- ! List token
sexprToAst (ListToken (x : xs) : ys) = do
    AST [sexprToAst (x : xs)] <> sexprToAst ys
-- ! Other token
sexprToAst (_ : xs) = do
    sexprToAst xs

-- INFO: Main function
checkSyntax :: [Token] -> IO ()
-- check if nb of open parenthesis == nb of close parenthesis
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
        tokenList <- parseFile file 0
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
