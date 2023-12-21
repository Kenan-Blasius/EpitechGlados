module Main (main) where

import System.Environment
import Types
import Eval

-- INFO: Create token list
parseLine :: String -> [Token]
parseLine str = do
    -- for each word in the string generate the tokens
    case words str of
        [] -> []
        ("define":xs) -> DefineToken : parseLine (" " ++ unwords xs)
        ("if":xs) -> IfToken : parseLine (" " ++ unwords xs)
        ("lambda":xs) -> LambdaToken : parseLine (" " ++ unwords xs)
        (_:_) -> do
            -- for each char in the string generate the tokens
            case str of
                [] -> []
                (' ':ys) -> SpaceToken : parseLine ys
                ('(':ys) -> OpenParenthesis : parseLine ys
                (')':ys) -> CloseParenthesis : parseLine ys
                ('\"':ys) -> do
                    let (quoteStr, rest) = span (/= '\"') ys
                    SymbolToken quoteStr : parseLine (tail rest)
                (y:ys) | y `elem` ['0'..'9'] -> IntToken (read [y]) : parseLine ys
                (y:ys) -> SymbolToken [y] : parseLine ys

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
parseFile (File (x:xs)) = mergeSymbols (parseLine x) ++ parseFile (File xs)

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
sexprToAst (IntToken x : ListToken y : _) = AST [IntAST x, sexprToAst y]
sexprToAst (IntToken x : xs) = AST [IntAST x] <> sexprToAst xs
-- ! Symbol token
sexprToAst (SymbolToken x : ListToken y : _) = AST [SymbolAST x, sexprToAst y]
sexprToAst (SymbolToken x : xs) = AST [SymbolAST x] <> sexprToAst xs
-- ! List token
sexprToAst (ListToken (x : xs) : ys) = AST [sexprToAst (x : xs)] <> sexprToAst ys
-- ! Other token
sexprToAst (_ : xs) = sexprToAst xs

-- INFO: Main function
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            putStrLn $ "Running file: " ++ filename
            contents <- readFile filename
            let file = File (lines contents)
            putStrLn "------------------------------------"
            print file
            putStrLn "------------------------------------"
            print (parseFile file)
            putStrLn "------------------------------------"
            let tokenList = parseFile file
            print (tokenListToSexpr tokenList)
            putStrLn "------------------------------------"
            let sexpr = tokenListToSexpr tokenList
            putStrLn $ printAST $ sexprToAst sexpr
            putStrLn "------------------------------------"
            let ast = sexprToAst sexpr
            let env = []
            print (evalAST env ast)
        _ -> putStrLn "No file given as an argument"
