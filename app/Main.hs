module Main (main) where

import System.Environment
import System.Exit
import Lib

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
         deriving Show

-- INFO: Create token list
parseLine :: String -> [Token]
parseLine str = do
    -- for each word in the string generate the tokens
    case words str of
        [] -> do
            []
        ("define":xs) -> do
            [DefineToken] ++ parseLine (" " ++ unwords xs)
        ("if":xs) -> do
            [IfToken] ++ parseLine (" " ++ unwords xs)
        ("lambda":xs) -> do
            [LambdaToken] ++ parseLine (" " ++ unwords xs)
        (_:_) -> do
            -- for each char in the string generate the tokens
            case str of
                [] -> do
                    []
                (' ':ys) -> do
                    [SpaceToken] ++ parseLine ys
                ('(':ys) -> do
                    [OpenParenthesis] ++ parseLine ys
                (')':ys) -> do
                    [CloseParenthesis] ++ parseLine ys
                (y:ys) | y `elem` ['0'..'9'] -> do
                    [IntToken (read [y])] ++ parseLine ys
                (y:ys) -> do
                    [SymbolToken [y]] ++ parseLine ys

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

parseFile :: File -> [Token]
parseFile (File []) = []
parseFile (File (x:xs)) = do
    (mergeSymbols (parseLine x)) ++ parseFile (File xs)

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
sexprToAst [] = AST []
-- TODO If token
sexprToAst (ListToken (IfToken : xs) : ys) = do
    let (cond, rest) = getSubList xs
    let (expr1, rest2) = getSubList rest
    let (expr2, rest3) = getSubList rest2
    IfAST (sexprToAst cond) (sexprToAst expr1) (sexprToAst expr2)

-- TODO Define token
sexprToAst (ListToken (DefineToken : xs) : ys) = do
    let (name, rest) = (head xs, tail xs)
    let (expr, rest2) = getSubList rest
    DefineAST (show name) (sexprToAst expr)

-- TODO Lambda token
sexprToAst (ListToken (LambdaToken : xs) : ys) = do
    let (args, body) = ([head xs], tail xs)
    LambdaAST (sexprToAst args) (sexprToAst body)

-- Int token
sexprToAst (ListToken (IntToken x : xs) : ys) = do
    IntAST x
-- Symbol token
sexprToAst (ListToken (SymbolToken x : xs) : ys) = do
    SymbolAST x
-- List token
sexprToAst (ListToken (x : xs) : ys) = do
    sexprToAst (ListToken (x : xs) : ys)
-- Other token
sexprToAst (x : xs) = do
    sexprToAst xs

-- INFO: Main function
main :: IO ()
main = do
    -- Run the file given as an argument
    args <- getArgs
    case args of
        [filename] -> do
            putStrLn $ "Running file: " ++ filename
            contents <- readFile filename
            file <- return $ File (lines contents)
            putStrLn "------------------------------------"
            putStrLn $ show file
            putStrLn "------------------------------------"
            putStrLn $ show $ parseFile file
            putStrLn "------------------------------------"
            let tokenList = parseFile file
            putStrLn $ show $ tokenListToSexpr tokenList
            putStrLn "------------------------------------"
            let sexpr = tokenListToSexpr tokenList
            putStrLn $ show $ sexprToAst sexpr
            putStrLn "------------------------------------"
        _ -> do
            putStrLn "No file given as an argument"
