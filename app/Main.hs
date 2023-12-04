module Main (main) where

import System.Environment
import Types
import Debug.Trace

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

indent :: Int -> String
indent 0 = ""
indent n = "|   " ++ indent (n - 1)

printAST :: AST -> String
printAST ast = printASTIndented 0 ast
    where
        printASTIndented :: Int -> AST -> String
        printASTIndented depth DeadLeafAST = indent depth ++ "DeadLeafAST\n"
        printASTIndented depth (IntAST value) = indent depth ++ "IntAST " ++ show value ++ "\n"
        printASTIndented depth (SymbolAST name) = indent depth ++ "SymbolAST " ++ name ++ "\n"
        printASTIndented depth (DefineAST name expr) =
            indent depth ++ "DefineAST " ++ name ++ "\n" ++ printASTIndented (depth + 1) expr
        printASTIndented depth (LambdaAST args body) =
            indent depth ++ "LambdaAST\n" ++ printASTIndented (depth + 1) args ++ printASTIndented (depth + 1) body
        printASTIndented depth (IfAST cond expr1 expr2) =
            indent depth ++ "IfAST\n" ++
                printASTIndented (depth + 1) cond ++
                printASTIndented (depth + 1) expr1 ++
                printASTIndented (depth + 1) expr2
        printASTIndented depth (AST astList) =
            indent depth ++ "AST\n" ++ concatMap (printASTIndented (depth + 1)) astList

evalAST :: AST -> AST
evalAST (AST []) = DeadLeafAST
evalAST (AST (SymbolAST "=" : x : y : _)) =
    trace ("SymbolAST =: " ++ show x ++ " " ++ show y) $
    case (evalAST x, evalAST y) of
        (IntAST x', IntAST y') ->
            trace ("SymbolAST =: " ++ show x' ++ " " ++ show y') $
            if x' == y' then IntAST 1 else IntAST 0
        _ -> trace ("SymbolAST =: " ++ show x ++ " " ++ show y) DeadLeafAST
evalAST (AST (x:xs)) =
    trace ("AST: " ++ show x) $ evalAST x
evalAST (IfAST cond expr1 expr2) =
    trace ("IfAST: Condition = " ++ printAST cond) $
    case evalAST cond of
        IntAST 0 -> trace "IfAST: Condition is 0, evaluating expr2" $ evalAST expr2
        IntAST _ -> trace "IfAST: Condition is non-zero, evaluating expr1" $ evalAST expr1
        _ -> trace "IfAST: Condition is not an integer, returning DeadLeafAST" DeadLeafAST
evalAST (DefineAST name expr) =
    trace ("Defining: " ++ name) $ evalAST expr
evalAST (LambdaAST args body) =
    trace ("Lambda: " ++ printAST args) $ evalAST body
evalAST (IntAST x) = IntAST x
evalAST (SymbolAST x) =
    trace ("Symbol: " ++ x) DeadLeafAST

-- INFO: Main function
main :: IO ()
main = do
    -- Hardcoded test
    let ast = IfAST (AST [SymbolAST "=", IntAST 0, IntAST 1]) (AST [IntAST 1]) (AST [IntAST 2])
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
