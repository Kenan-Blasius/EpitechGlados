import System.Environment

import Types
import Debug.Trace

import qualified Data.ByteString.UTF8 as UTF8
import Data.ByteString (unpack)

import Data.Word (Word8)
import Data.List
import Data.Maybe
import Data.Char
import Debug.Trace


-- ; Opcode Definitions
-- LOAD_CONST      0x01
-- LOAD_VAR        0x02
-- STORE_VAR       0x03
-- BINARY_OP       0x04
-- UNARY_OP        0x05
-- COMPARE_OP      0x06
-- JUMP_IF_TRUE    0x07
-- JUMP_IF_FALSE   0x08
-- JUMP            0x09
-- POP             0x0A
-- DUP             0x0B
-- CALL            0x0C
-- RETURN          0x0D
-- BUILD_LIST      0x0E
-- INDEX           0x0F
-- ATTRIBUTE       0x10
-- CREATE_OBJECT   0x11

forEachValue :: [Word8] -> [Word8]
forEachValue [] = []
forEachValue (x:xs) = x : forEachValue xs
--   where
--     tracedX = trace ("Processing: " ++ show x) x

stringToWord8 :: String -> [Word8]
stringToWord8 str = unpack (UTF8.fromString str)

-- open file given in argument, and print it
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            contents <- readFile filename
            print (stringToWord8 contents)
            forEachValue (stringToWord8 contents)

        _ -> do
            putStrLn "No file given as an argument"
