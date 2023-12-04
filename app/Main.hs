module Main (main) where

import System.Environment
import Types
import Debug.Trace
import Eval

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
sexprToAst (ListToken (IfToken : xs) : _) = do
    let (cond, rest) = getSubList xs
    let (expr1, rest2) = getSubList rest
    let (expr2, _) = getSubList rest2
    IfAST (sexprToAst cond) (sexprToAst expr1) (sexprToAst expr2)

-- TODO Define token
sexprToAst (ListToken (DefineToken : xs) : _) = do
    let (name, rest) = (head xs, tail xs)
    let (expr, _) = getSubList rest
    DefineAST (show name) (sexprToAst expr)

-- TODO Lambda token
sexprToAst (ListToken (LambdaToken : xs) : _) = do
    let (args, body) = ([head xs], tail xs)
    LambdaAST (sexprToAst args) (sexprToAst body)

-- Int token
sexprToAst (ListToken (IntToken x : _) : _) = do
    IntAST x
-- Symbol token
sexprToAst (ListToken (SymbolToken x : _) : _) = do
    SymbolAST x
-- List token
sexprToAst (ListToken (x : xs) : ys) = do
    sexprToAst (ListToken (x : xs) : ys)
-- Other token
sexprToAst (_ : xs) = do
    sexprToAst xs


-- INFO: Main function
main :: IO ()
main = do
    -- Hardcoded test
    let ast = IfAST (AST [SymbolAST "=", SymbolAST "1", IntAST 1]) (AST [SymbolAST "a"]) (AST [IntAST 2])
    putStrLn $ "AST: " ++ printAST ast
    putStrLn $ "Result: " ++ printAST (evalAST ast)


{-
-- Kenan's code
AST: IfAST (SymbolAST "=") (IntAST 0) (IntAST 2)
evalAST IfAST (SymbolAST "=") (IntAST 0) (IntAST 2)
evalAST SymbolAST "="
Symbol: =
Result: Nothing
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
            let ast = sexprToAst sexpr
            putStrLn $ show $ evalAST ast
        _ -> do
            putStrLn "No file given as an argument"
-}
