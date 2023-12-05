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
                ('\"':ys) -> do
                    let (quoteStr, rest) = span (/= '\"') ys
                    [SymbolToken quoteStr] ++ parseLine (tail rest)
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
main :: IO ()
main = do
--     -- Hardcoded test
--     let ast = IfAST (AST [SymbolAST ">=", SymbolAST "103", IntAST 103]) (AST [SymbolAST "True"]) (AST [IntAST 0])
--     putStrLn $ "AST: " ++ printAST ast
--     putStrLn $ "Result: " ++ printAST (evalAST ast)


-- Kenan's code
-- AST: IfAST (SymbolAST "=") (IntAST 0) (IntAST 2)
-- evalAST IfAST (SymbolAST "=") (IntAST 0) (IntAST 2)
-- evalAST SymbolAST "="
-- Symbol: =
-- Result: Nothing
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
            putStrLn $ printAST $ sexprToAst sexpr
            putStrLn "------------------------------------"
            let ast = sexprToAst sexpr
            putStrLn $ show $ evalAST ast
        _ -> do
            putStrLn "No file given as an argument"
