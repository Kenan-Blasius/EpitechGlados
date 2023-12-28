module ParserModule (
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
