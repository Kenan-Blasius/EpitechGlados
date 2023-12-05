module Main (main) where

import System.Environment
import System.Exit
import System.IO
import Types
import Eval

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
            tokenList <- parseFile file 0
            putStrLn $ show $ tokenList
            putStrLn "------------------------------------"
            putStrLn $ show $ tokenListToSexpr tokenList
            putStrLn "------------------------------------"
            let sexpr = tokenListToSexpr tokenList
            putStrLn $ printAST $ sexprToAst sexpr
            putStrLn "------------------------------------"
            let ast = sexprToAst sexpr
            putStrLn $ show $ evalAST ast
        _ -> do
            putStrLn "No file given as an argument"
