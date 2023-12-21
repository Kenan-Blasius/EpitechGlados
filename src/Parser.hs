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

-- Test all parsers
parsingBootstrap :: IO ()
parsingBootstrap = do
    putStrLn "------------------------------------"
    putStrLn "Parsing bootstrap"
    putStrLn "------------------------------------"
    putStrLn ""
    putStrLn "parseChar"
    putStrLn $ show $ runParser (parseChar 'a') "abcd"
    putStrLn $ show $ runParser (parseChar 'z') "abcd"
    putStrLn $ show $ runParser (parseChar 'b') "abcd"
    putStrLn $ show $ runParser (parseChar 'a') "aaaa"
    putStrLn ""
    putStrLn "parseString"
    putStrLn $ show $ runParser (parseString "abc") "abcd"
    putStrLn $ show $ runParser (parseString "abc") "abce"
    putStrLn $ show $ runParser (parseString "abc") "ab"
    putStrLn $ show $ runParser (parseString "abc") "abc"
    putStrLn $ show $ runParser (parseString "abc") "abcabc"
    putStrLn $ show $ runParser (parseString "abc") "bca"
    putStrLn ""
    putStrLn "parseAnyChar"
    putStrLn $ show $ runParser (parseAnyChar "bca") "abcd"
    putStrLn $ show $ runParser (parseAnyChar "xyz") "abcd"
    putStrLn $ show $ runParser (parseAnyChar "bca") "cdef"
    putStrLn ""
    putStrLn "parseOr"
    putStrLn $ show $ runParser (parseOr (parseChar 'a') (parseChar 'b')) "abcd"
    putStrLn $ show $ runParser (parseOr (parseChar 'a') (parseChar 'b')) "bcda"
    putStrLn $ show $ runParser (parseOr (parseChar 'a') (parseChar 'b')) "xyz"
    putStrLn ""
    putStrLn "parseAnd"
    putStrLn $ show $ runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd"
    putStrLn $ show $ runParser (parseAnd (parseChar 'a') (parseChar 'b')) "bcda"
    putStrLn $ show $ runParser (parseAnd (parseChar 'a') (parseChar 'b')) "acd"
    putStrLn ""
    putStrLn "parseAndWith"
    putStrLn $ show $ runParser (parseAndWith (\ x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "abcd"
    putStrLn $ show $ runParser (parseAndWith (\ x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "bcda"
    putStrLn ""
    putStrLn "parseMany"
    putStrLn $ show $ runParser (parseMany (parseChar ' ')) "    foobar"
    putStrLn $ show $ runParser (parseMany (parseChar ' ')) "foobar    "
    putStrLn ""
    putStrLn "parseSome"
    putStrLn $ show $ runParser (parseSome (parseAnyChar ['0' .. '9'])) "42foobar"
    putStrLn $ show $ runParser (parseSome (parseAnyChar ['0' .. '9'])) "foobar42"
    putStrLn ""
    putStrLn "parseUInt"
    putStrLn $ show $ runParser parseUInt "42foobar"
    putStrLn $ show $ runParser parseUInt "-42foobar"
    putStrLn $ show $ runParser parseUInt "+42foobar"
    putStrLn $ show $ runParser parseUInt "foobar42"
    putStrLn ""
    putStrLn "parseInt"
    putStrLn $ show $ runParser parseInt "42foobar"
    putStrLn $ show $ runParser parseInt "-42foobar"
    putStrLn $ show $ runParser parseInt "+42foobar"
    putStrLn $ show $ runParser parseInt "foobar42"
    putStrLn ""
    putStrLn "parsePair"
    putStrLn $ show $ runParser (parsePair parseInt) "(1 2)"
    putStrLn $ show $ runParser (parsePair parseInt) "(1 2 3)"
    putStrLn $ show $ runParser (parsePair parseInt) "(1)"
    putStrLn $ show $ runParser (parsePair parseInt) "(1"
    putStrLn $ show $ runParser (parsePair parseInt) "1 2)"
    putStrLn $ show $ runParser (parsePair parseInt) "1 2"
    putStrLn $ show $ runParser (parsePair parseInt) "1"
    putStrLn $ show $ runParser (parsePair parseInt) ""
    putStrLn ""
    putStrLn "parseOrBoth"
    putStrLn $ show $ runParser (parseOrBoth (parseChar '4') parseInt) "42foobar"
    putStrLn $ show $ runParser (parseOrBoth parseInt (parseChar '4')) "42foobar"
    putStrLn $ show $ runParser (parseOrBoth parseInt (parseChar 'a')) "42foobar"
    putStrLn $ show $ runParser (parseOrBoth (parseChar 'a') (parseChar '4')) "42foobar"
    putStrLn $ show $ runParser (parseOrBoth (parseChar 'a') (parseChar 'b')) "42foobar"
    putStrLn ""
    putStrLn "parseList"
    putStrLn $ show $ runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "(1 2)"
    putStrLn $ show $ runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "(1 2 3)alut"
    putStrLn $ show $ runParser (parseList (parseChar '(') (parseChar ',') (parseChar ')') (parseChar ' ') parseInt) "(1,  2,  3)"
    putStrLn $ show $ runParser (parseList (parseChar '(') (parseChar ',') (parseChar ')') (parseChar ' ') parseInt) "alut(1,2,3)"
    putStrLn $ show $ runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "(1)"
    putStrLn $ show $ runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "(1"
    putStrLn $ show $ runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "1 2)"
    putStrLn $ show $ runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "1 2"
    putStrLn $ show $ runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "1"
    putStrLn $ show $ runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) ""
    putStrLn ""
    putStrLn "------------------------------------"

-- INFO: Create token list
parseLine :: String -> Int -> IO ([Token])
parseLine str lineNumber = do
    -- for each word in the string generate the tokens
    case words str of
        [] -> do
            return ([])
        ("define":xs) -> do
            tmpDefine <- parseLine (" " ++ unwords xs) lineNumber
            return ([DefineToken] ++ tmpDefine)
        ("if":xs) -> do
            tmpIf <- parseLine (" " ++ unwords xs) lineNumber
            return ([IfToken] ++ tmpIf)
        ("lambda":xs) -> do
            tmpLambda <- parseLine (" " ++ unwords xs) lineNumber
            return ([LambdaToken] ++ tmpLambda)
        (_:_) -> do
            -- for each char in the string generate the tokens
            case str of
                [] -> do
                    return ([])
                (' ':ys) -> do
                    tmpSpace <- parseLine ys lineNumber
                    return ([SpaceToken] ++ tmpSpace)
                ('(':ys) -> do
                    tmpOpenParenthesis <- parseLine ys lineNumber
                    return ([OpenParenthesis] ++ tmpOpenParenthesis)
                (')':ys) -> do
                    tmpCloseParenthesis <- parseLine ys lineNumber
                    return ([CloseParenthesis] ++ tmpCloseParenthesis)
                ('\"':ys) -> do
                    let (quoteStr, rest) = span (/= '\"') ys
                    if null rest then do
                        hPutStrLn stderr $ "Error: Missing \" at line " ++ show lineNumber
                        exitWith (ExitFailure 84)
                    else do
                        tmpQuoteStr <- parseLine (tail rest) lineNumber
                        return ([SymbolToken quoteStr] ++ tmpQuoteStr)
                (y:ys) | y `elem` ['0'..'9'] -> do
                    tmpInt <- parseLine ys lineNumber
                    return ([IntToken (read [y])] ++ tmpInt)
                (y:ys) -> do
                    tmpSymbol <- parseLine ys lineNumber
                    return ([SymbolToken [y]] ++ tmpSymbol)

mergeSymbols :: [Token] -> [Token]
mergeSymbols [] = []
-- merge all consecutive symbols (ex: b o n j o u r  -> bonjour)
mergeSymbols (SymbolToken x : SymbolToken y : xs) = mergeSymbols (SymbolToken (x ++ y) : xs)
-- merge all consecutive numbers (ex: 1 2 3 -> 123)
mergeSymbols (IntToken x : IntToken y : xs) = mergeSymbols (IntToken (x * 10 + y) : xs)
-- -- Trim all spaces
-- mergeSymbols (SpaceToken : SpaceToken : xs) = mergeSymbols (SpaceToken : xs)
-- Delete all spaces
mergeSymbols (SpaceToken : xs) = mergeSymbols xs
-- No merge needed
mergeSymbols (x:xs) = x : mergeSymbols xs

parseFile :: File -> Int -> IO ([Token])
parseFile (File []) _ = return ([])
parseFile (File (x:xs)) lineNumber = do
    parsedLine <- parseLine x lineNumber
    rest <- parseFile (File xs) (lineNumber + 1)
    return ((mergeSymbols parsedLine) ++ rest)

-- INFO: Convert token list to SExpr
getSubList :: [Token] -> ([Token], [Token])
getSubList [] = ([], [])
getSubList (CloseParenthesis : xs) = ([], xs)
getSubList (OpenParenthesis : xs) = do
    let (subList, rest) = getSubList xs
    let (subList2, rest2) = getSubList rest
    (OpenParenthesis : subList ++ CloseParenthesis : subList2, rest2)
getSubList (x:xs) = do
    let (subList, rest) = getSubList xs
    (x : subList, rest)

tokenListToSexpr :: [Token] -> [Token]
tokenListToSexpr [] = []
-- all between parenthesis is a sub list of tokens
tokenListToSexpr (OpenParenthesis : xs) = do
    let (subList, rest) = getSubList xs
    ListToken (tokenListToSexpr subList) : tokenListToSexpr rest
-- all other tokens are converted to SExpr
tokenListToSexpr (x:xs) = x : tokenListToSexpr xs

-- INFO: Convert SExpr to AST
sexprToAst :: [Token] -> AST
sexprToAst [] = DeadLeafAST
-- ! If token
sexprToAst (IfToken : xs) = do
    let (cond, expr1, expr2) = (head xs, head (tail xs), head (tail (tail xs)))
    let cond2 = case cond of
            ListToken x -> x
            _ -> [cond]
    let expr12 = case expr1 of
            ListToken x -> x
            _ -> [expr1]
    let expr22 = case expr2 of
            ListToken x -> x
            _ -> [expr2]
    IfAST (sexprToAst cond2) (sexprToAst expr12) (sexprToAst expr22)
-- ! Define token
sexprToAst (DefineToken : xs) = do
    let (name, rest) = (head xs, tail xs)
    let (expr, _) = getSubList rest
    AST [DefineAST (show name) (sexprToAst expr)]
-- ! Lambda token
sexprToAst (LambdaToken : xs) = do
    let (args, body) = (head xs, tail xs)
    let args2 = case args of
            ListToken x -> x
            _ -> [args]
    LambdaAST (sexprToAst args2) (sexprToAst body)
-- ! Int token
sexprToAst (IntToken x : ListToken y : _) = do
    AST [IntAST x, sexprToAst y]
sexprToAst (IntToken x : xs) = do
    AST [IntAST x] <> sexprToAst xs
-- ! Symbol token
sexprToAst (SymbolToken x : ListToken y : _) = do
    AST [SymbolAST x, sexprToAst y]
sexprToAst (SymbolToken x : xs) = do
    AST [SymbolAST x] <> sexprToAst xs
-- ! List token
sexprToAst (ListToken (x : xs) : ys) = do
    AST [sexprToAst (x : xs)] <> sexprToAst ys
-- ! Other token
sexprToAst (_ : xs) = do
    sexprToAst xs

-- INFO: Main function
parser :: File -> IO (AST)
parser file = do
    parsingBootstrap
    putStrLn "------------------------------------"
    putStrLn $ show file
    putStrLn "------------------------------------"
    tokenList <- parseFile file 0
    putStrLn $ show $ tokenList
    putStrLn "------------------------------------"
    putStrLn $ show $ tokenListToSexpr tokenList
    putStrLn "------------------------------------"
    let sexpr = tokenListToSexpr tokenList
    putStrLn $ printAST $ sexprToAst sexpr
    putStrLn "------------------------------------"
    let ast = sexprToAst sexpr
    return (ast)
