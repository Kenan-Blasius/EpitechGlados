module ParserToken (
    getAbsolutePath,
    cleanFile,

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
) where

import ParserModule
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

-- Define the function to clean the file from all comments
cleanFile :: File -> Bool -> File
cleanFile (File []) _ = File []
cleanFile (File (x:xs)) inComment = do
    let (stillInComment, line) = cleanLine x inComment
    File ([line] ++ (getFileArray (cleanFile (File xs) stillInComment)))
    where
        cleanLine :: String -> Bool -> (Bool, String)
        cleanLine [] isInComment = (isInComment, [])
        cleanLine (y:ys) isInComment | isInComment == False && (length ys) > 0 && y == '/' && head ys == '/' = (isInComment, [])
                                     | isInComment == False && (length ys) > 0 && y == '/' && head ys == '*' = (True, (snd (cleanLine (tail ys) True)))
                                     | isInComment == True  && (length ys) > 0 && y == '*' && head ys == '/' = (False, " " ++ (snd (cleanLine (tail ys) False)))
                                     | isInComment == True                                                   = (isInComment, (snd (cleanLine ys isInComment)))
                                     | otherwise                                                             = (isInComment, [y] ++ (snd (cleanLine ys isInComment)))
        getFileArray :: File -> [String]
        getFileArray (File y) = y

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
            num2 <- runParser parseUInt (snd dot)
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
            Just (_, ys) -> case runParser (parseMany (parseEscapedChar <|> parseAnyChar [chr i | i <- [0x0000 .. 0x10FFFF], chr i /= '\"'])) ys of
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
            Just (_, ys) -> case runParser (parseEscapedChar <|> parseAnyChar [chr i | i <- [0x0000 .. 0x10FFFF], chr i /= '\'']) ys of
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
    <|> (parseKeyword "return" ReturnToken)
    <|> (parseKeyword ":" FunTypeToken)
    -- Types
    <|> (parseKeyword "int" IntTypeToken)
    <|> (parseKeyword "float" FloatTypeToken)
    <|> (parseKeyword "char" CharTypeToken)
    <|> (parseKeyword "string" StringTypeToken)
    <|> (parseKeyword "void" VoidTypeToken)
    -- -- Comments
    -- <|> (parseKeyword "/*" CommentStart)
    -- <|> (parseKeyword "*/" CommentEnd)
    -- <|> (parseKeyword "//" InlineCommentStart)
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
parseLine :: String -> Int -> String -> String -> IO ([Token])
parseLine str lineNumber filename originalStr =
    catch (
        -- for each word in the string generate the tokens
        case runParser (parseMany parseToken) str of
            Just (x, _) -> do
                return (x)
            Nothing -> do
                throw (ParserError ("Invalid syntax on file: " ++ show filename ++ " at line " ++ show lineNumber ++ ": " ++ originalStr))
        ) handler
    where
        handler :: ParserError -> IO ([Token])
        handler e = do
            throw (ParserError ("Invalid syntax on file: " ++ show filename ++ " at line " ++ show lineNumber ++ ":\n" ++ originalStr ++ "\n" ++ show e))

includeFile :: String -> [String] -> IO ([Token])
includeFile str filenames = do
    contents <- readFile str
    file <- return $ File (lines contents)
    putStrLn "File included: "
    putStrLn str
    putStrLn "------------------------------------"
    putStrLn $ show file
    putStrLn "------------------------------------"
    putStrLn $ show $ cleanFile file False
    putStrLn "------------------------------------"
    let cleanedFile = cleanFile file False
    parseFile cleanedFile 1 filenames file

mergeSymbols :: [Token] -> [String] -> IO [Token]
mergeSymbols [] _ = return []
-- -- Once we found a InlineCommentStart we ignore all the rest of the line
-- mergeSymbols (InlineCommentStart : _) _ = return []
-- include a file
mergeSymbols (IncludeToken : xs) filenames | (length (filter (/= SpaceToken) xs)) > 0 && head (filter (/= SpaceToken) xs) == StringToken str = do
    filename <- getAbsolutePath str
    rest <- mergeSymbols (tail (dropWhile (/= StringToken str) xs)) (filename : filenames)
    if filename `elem` filenames then do
        -- throw (ParserError ("Error: Recursive include of file " ++ str))
        return (rest)
    else do
        includedFile <- includeFile filename (filename : filenames)
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
mergeSymbols (SymbolToken x : VoidTypeToken : xs) filenames = mergeSymbols (SymbolToken (x ++ "void") : xs) filenames
mergeSymbols (SymbolToken x : IfToken : xs) filenames = mergeSymbols (SymbolToken (x ++ "if") : xs) filenames
mergeSymbols (SymbolToken x : ElseToken : xs) filenames = mergeSymbols (SymbolToken (x ++ "else") : xs) filenames
mergeSymbols (SymbolToken x : FunToken : xs) filenames = mergeSymbols (SymbolToken (x ++ "fun") : xs) filenames
mergeSymbols (SymbolToken x : ForToken : xs) filenames = mergeSymbols (SymbolToken (x ++ "for") : xs) filenames
mergeSymbols (SymbolToken x : WhileToken : xs) filenames = mergeSymbols (SymbolToken (x ++ "while") : xs) filenames
mergeSymbols (SymbolToken x : ReturnToken : xs) filenames = mergeSymbols (SymbolToken (x ++ "return") : xs) filenames
mergeSymbols (SymbolToken x : IntToken y : xs) filenames = mergeSymbols (SymbolToken (x ++ show y) : xs) filenames

mergeSymbols (IntTypeToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("int" ++ x) : xs) filenames
mergeSymbols (FloatTypeToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("float" ++ x) : xs) filenames
mergeSymbols (CharTypeToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("char" ++ x) : xs) filenames
mergeSymbols (StringTypeToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("string" ++ x) : xs) filenames
mergeSymbols (VoidTypeToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("void" ++ x) : xs) filenames
mergeSymbols (IfToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("if" ++ x) : xs) filenames
mergeSymbols (ElseToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("else" ++ x) : xs) filenames
mergeSymbols (FunToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("fun" ++ x) : xs) filenames
mergeSymbols (ForToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("for" ++ x) : xs) filenames
mergeSymbols (WhileToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("while" ++ x) : xs) filenames
mergeSymbols (ReturnToken : SymbolToken x : xs) filenames = mergeSymbols (SymbolToken ("return" ++ x) : xs) filenames
mergeSymbols (IntToken x : SymbolToken y : xs) filenames = mergeSymbols (SymbolToken (show x ++ y) : xs) filenames

-- merge else if to elif
mergeSymbols (ElseToken : xs) filenames | (length (filter (/= SpaceToken) xs)) > 0 && (head (filter (/= SpaceToken) xs)) == IfToken = do
    mergeSymbols (ElseIfToken : (tail (dropWhile (/= IfToken) xs))) filenames
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

parseFile :: File -> Int -> [String] -> File -> IO ([Token])
parseFile (File []) _ _ _ = return ([])
parseFile _ _ _ (File []) = return ([])
parseFile (File (x:xs)) lineNumber filenames (File (ox:oxs)) = do
    parsedLine <- parseLine x lineNumber (head filenames) ox
    currentLine <- mergeSymbols parsedLine filenames
    rest <- parseFile (File xs) (lineNumber + 1) filenames (File oxs)
    return (currentLine ++ rest)

-- INFO: Convert token list to SExpr
getSubList :: Token -> Token -> [Token] -> ([Token], [Token])
getSubList _ _ [] = ([], [])
-- -- Comment case
-- getSubList CommentStart CommentEnd (x:xs) | x == CommentEnd = ([], xs)
-- getSubList CommentStart CommentEnd (_:xs) = getSubList CommentStart CommentEnd xs
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
-- -- all between comment is ignored
-- tokenListToSexpr (CommentStart : xs) = do
--     let (_, rest) = getSubList CommentStart CommentEnd xs
--     tokenListToSexpr rest
-- Fix minus token being assigned to the number even if it should be a binary operator
tokenListToSexpr (SymbolToken x : IntToken y : xs) | y < 0 = do
    (SymbolToken x : MinusToken : IntToken (-y) : tokenListToSexpr xs)
tokenListToSexpr (SymbolToken x : FloatToken y : xs) | y < 0 = do
    (SymbolToken x : MinusToken : FloatToken (-y) : tokenListToSexpr xs)
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
