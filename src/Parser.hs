module Parser (
    parser,
) where

import ParserToken
import ParserAST
import System.Exit
import System.IO
import Types
import Control.Exception
-- import Debug.Trace

-- INFO: Main function
checkSyntax :: [Token] -> IO ()
checkSyntax xs | IncludeToken `elem` xs = throw $ (ParserError "Error: #include token must be followed by a string for the file to include")
                | length (filter (== OpenParenthesis) xs) < length (filter (== CloseParenthesis) xs) =  throw $ (ParserError "Error: Missing ( token")
                | length (filter (== OpenParenthesis) xs) > length (filter (== CloseParenthesis) xs) =  throw $ (ParserError "Error: Missing ) token")
                | length (filter (== OpenBracket) xs) < length (filter (== CloseBracket) xs) =          throw $ (ParserError "Error: Missing [ token")
                | length (filter (== OpenBracket) xs) > length (filter (== CloseBracket) xs) =          throw $ (ParserError "Error: Missing ] token")
                | length (filter (== OpenBraces) xs) < length (filter (== CloseBraces) xs) =            throw $ (ParserError "Error: Missing { token")
                | length (filter (== OpenBraces) xs) > length (filter (== CloseBraces) xs) =            throw $ (ParserError "Error: Missing } token")
                -- | length (filter (== CommentStart) xs) < length (filter (== CommentEnd) xs) =           throw $ (ParserError "Error: Missing /* token")
                -- | length (filter (== CommentStart) xs) > length (filter (== CommentEnd) xs) =           throw $ (ParserError "Error: Missing */ token")
                | otherwise = return ()

parser :: File -> String -> IO (AST)
parser file filename = do
    catch (
        do
        absoluteFilename <- getAbsolutePath filename
        putStrLn "Original file: "
        putStrLn absoluteFilename
        putStrLn "------------------------------------"
        putStrLn $ show file
        putStrLn "------------------------------------"
        putStrLn $ show $ cleanFile file False
        putStrLn "------------------------------------"
        let cleanedFile = cleanFile file False
        tokenList <- parseFile cleanedFile 1 [absoluteFilename]
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
