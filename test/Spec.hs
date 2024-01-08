import Parser
import ParserModule
import Control.Applicative
import ParserToken
import ParserAST
import Types
import Test.HUnit

-- ? Orphan instances for Semigroup and Monoid (needed for semigroupParserTest and monoidParserTest)
instance Semigroup Int where
    (<>) = (+)

instance Monoid Int where
    mempty = 0
    mappend = (<>)

functorParserTest :: Test
functorParserTest =
    TestList
    [
        TestCase (assertEqual "functorParser" (Just (1235, "")) (runParser (fmap (+1) parseInt) "1234")),
        TestCase (assertEqual "functorParser" Nothing (runParser (fmap (+1) parseInt) "abcd"))
    ]

applicativeParserTest :: Test
applicativeParserTest =
    TestList
    [
        TestCase (assertEqual "applicativeParser" (Just (1235, "")) (runParser (pure (+1) <*> parseInt) "1234")),
        TestCase (assertEqual "applicativeParser" Nothing (runParser (pure (+1) <*> parseInt) "abcd"))
    ]

alternativeParserTest :: Test
alternativeParserTest =
    TestList
    [
        TestCase (assertEqual "alternativeParser" (Just (-1234, "")) (runParser (parseUInt <|> parseInt) "-1234")),
        TestCase (assertEqual "alternativeParser" (Just (-1234, "")) (runParser (parseInt <|> parseUInt) "-1234")),
        TestCase (assertEqual "alternativeParser" Nothing (runParser (parseUInt <|> parseInt) "abcd")),
        TestCase (assertEqual "alternativeParser" Nothing (runParser (parseInt <|> parseUInt) "abcd"))
    ]

semigroupParserTest :: Test
semigroupParserTest =
    TestList
    [
        TestCase (assertEqual "semigroupParser" (Just (1234, "")) (runParser (parseUInt <> parseInt) "1234")),
        TestCase (assertEqual "semigroupParser" (Just (1234, "")) (runParser (parseInt <> parseUInt) "1234")),
        TestCase (assertEqual "semigroupParser" Nothing (runParser (parseUInt <> parseInt) "abcd")),
        TestCase (assertEqual "semigroupParser" Nothing (runParser (parseInt <> parseUInt) "abcd"))
    ]

monoidParserTest :: Test
monoidParserTest =
    TestList
    [
        TestCase (assertEqual "monoidParser" (Just (1234, "")) (runParser (mempty `mappend` parseUInt) "1234")),
        TestCase (assertEqual "monoidParser" (Just (1234, "")) (runParser (parseUInt `mappend` mempty) "1234")),
        TestCase (assertEqual "monoidParser" (Just (1234, "")) (runParser (mempty `mappend` parseInt) "1234")),
        TestCase (assertEqual "monoidParser" (Just (1234, "")) (runParser (parseInt `mappend` mempty) "1234")),
        TestCase (assertEqual "monoidParser" (Just (11, "")) (runParser (parseUInt `mappend` parseInt) "53-42")),
        TestCase (assertEqual "monoidParser" Nothing (runParser (mempty `mappend` parseUInt) "abcd")),
        TestCase (assertEqual "monoidParser" Nothing (runParser (parseUInt `mappend` mempty) "abcd")),
        TestCase (assertEqual "monoidParser" Nothing (runParser (mempty `mappend` parseInt) "abcd")),
        TestCase (assertEqual "monoidParser" Nothing (runParser (parseInt `mappend` mempty) "abcd"))
    ]

monadParserTest :: Test
monadParserTest =
    TestList
    [
        TestCase (assertEqual "monadParser" (Just (1235, "")) (runParser (parseInt >>= \ x -> return (x + 1)) "1234")),
        TestCase (assertEqual "monadParser" Nothing (runParser (parseInt >>= \ x -> return (x + 1)) "abcd"))
    ]

parseCharTest :: Test
parseCharTest =
    TestList
    [
        TestCase (assertEqual "parseChar" (Just ('a', "bcd"))           (runParser (parseChar 'a') "abcd")),
        TestCase (assertEqual "parseChar" Nothing                       (runParser (parseChar 'z') "bcd")),
        TestCase (assertEqual "parseChar" Nothing                       (runParser (parseChar 'b') "abcd")),
        TestCase (assertEqual "parseChar" (Just ('a', "aaa"))           (runParser (parseChar 'a') "aaaa"))
    ]

parseStringTest :: Test
parseStringTest =
    TestList
    [
        TestCase (assertEqual "parseString" (Just ("abc", "def"))       (runParser (parseString "abc") "abcdef")),
        TestCase (assertEqual "parseString" Nothing                     (runParser (parseString "abc") "defabc")),
        TestCase (assertEqual "parseString" (Just ("abc", "defg"))      (runParser (parseString "abc") "abcdefg")),
        TestCase (assertEqual "parseString" (Just ("abc", "abc"))       (runParser (parseString "abc") "abcabc")),
        TestCase (assertEqual "parseString" (Just ("abc", ""))          (runParser (parseString "abc") "abc")),
        TestCase (assertEqual "parseString" Nothing                     (runParser (parseString "abc") "ab"))
    ]

parseAnyCharTest :: Test
parseAnyCharTest =
    TestList
    [
        TestCase (assertEqual "parseAnyChar" (Just ('a', "bcd"))        (runParser (parseAnyChar "bca") "abcd")),
        TestCase (assertEqual "parseAnyChar" Nothing                    (runParser (parseAnyChar "xyz") "abcd")),
        TestCase (assertEqual "parseAnyChar" (Just ('c', "def"))        (runParser (parseAnyChar "bca") "cdef"))
    ]

parseOrTest :: Test
parseOrTest =
    TestList
    [
        TestCase (assertEqual "parseOr" (Just ('a', "bcd"))             (runParser (parseOr (parseChar 'a') (parseChar 'b')) "abcd")),
        TestCase (assertEqual "parseOr" (Just ('b', "cda"))             (runParser (parseOr (parseChar 'a') (parseChar 'b')) "bcda")),
        TestCase (assertEqual "parseOr" (Just ('a', "cd"))              (runParser (parseOr (parseChar 'a') (parseChar 'b')) "acd")),
        TestCase (assertEqual "parseOr" Nothing                         (runParser (parseOr (parseChar 'a') (parseChar 'b')) "defg"))
    ]

parseAndTest :: Test
parseAndTest =
    TestList
    [
        TestCase (assertEqual "parseAnd" (Just (('a', 'b'), "cd"))      (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "abcd")),
        TestCase (assertEqual "parseAnd" Nothing                        (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "bcda")),
        TestCase (assertEqual "parseAnd" Nothing                        (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "acd")),
        TestCase (assertEqual "parseAnd" Nothing                        (runParser (parseAnd (parseChar 'a') (parseChar 'b')) "defg"))
    ]

parseAndWithTest :: Test
parseAndWithTest =
    TestList
    [
        TestCase (assertEqual "parseAndWith" (Just ("ab", "cd"))        (runParser (parseAndWith (\x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "abcd")),
        TestCase (assertEqual "parseAndWith" Nothing                    (runParser (parseAndWith (\x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "bcda")),
        TestCase (assertEqual "parseAndWith" Nothing                    (runParser (parseAndWith (\x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "acd")),
        TestCase (assertEqual "parseAndWith" Nothing                    (runParser (parseAndWith (\x y -> [x,y]) (parseChar 'a') (parseChar 'b')) "defg"))
    ]

parseManyTest :: Test
parseManyTest =
    TestList
    [
        TestCase (assertEqual "parseMany" (Just ("aaaa", ""))           (runParser (parseMany (parseChar 'a')) "aaaa")),
        TestCase (assertEqual "parseMany" (Just ("aaa", "b"))           (runParser (parseMany (parseChar 'a')) "aaab")),
        TestCase (assertEqual "parseMany" (Just ("", "baaa"))           (runParser (parseMany (parseChar 'a')) "baaa")),
        TestCase (assertEqual "parseMany" (Just ("", "baab"))           (runParser (parseMany (parseChar 'a')) "baab")),
        TestCase (assertEqual "parseMany" (Just ("    ", "foobar"))     (runParser (parseMany (parseChar ' ')) "    foobar")),
        TestCase (assertEqual "parseMany" (Just ("", "foobar    "))     (runParser (parseMany (parseChar ' ')) "foobar    "))
    ]

parseSomeTest :: Test
parseSomeTest =
    TestList
    [
        TestCase (assertEqual "parseSome" (Just ("aaaa", ""))           (runParser (parseSome (parseChar 'a')) "aaaa")),
        TestCase (assertEqual "parseSome" (Just ("aaa", "b"))           (runParser (parseSome (parseChar 'a')) "aaab")),
        TestCase (assertEqual "parseSome" Nothing                       (runParser (parseSome (parseChar 'a')) "baaa")),
        TestCase (assertEqual "parseSome" Nothing                       (runParser (parseSome (parseChar 'a')) "baab")),
        TestCase (assertEqual "parseSome" (Just ("42", "foobar"))       (runParser (parseSome (parseAnyChar ['0' .. '9'])) "42foobar")),
        TestCase (assertEqual "parseSome" Nothing                       (runParser (parseSome (parseAnyChar ['0' .. '9'])) "foobar42"))
    ]

parseUIntTest :: Test
parseUIntTest =
    TestList
    [
        TestCase (assertEqual "parseUInt" (Just (1234, ""))             (runParser parseUInt "1234")),
        TestCase (assertEqual "parseUInt" (Just (1234, "foobar"))       (runParser parseUInt "1234foobar")),
        TestCase (assertEqual "parseUInt" Nothing                       (runParser parseUInt "foobar")),
        TestCase (assertEqual "parseUInt" (Just (0, ""))                (runParser parseUInt "0")),
        TestCase (assertEqual "parseUInt" (Just (0, ""))                (runParser parseUInt "00")),
        TestCase (assertEqual "parseUInt" (Just (1, ""))                (runParser parseUInt "01")),
        TestCase (assertEqual "parseUInt" (Just (1, ""))                (runParser parseUInt "0001")),
        TestCase (assertEqual "parseUInt" (Just (0, ""))                (runParser parseUInt "0000")),
        TestCase (assertEqual "parseUInt" (Just (42, "foobar"))         (runParser parseUInt "42foobar")),
        TestCase (assertEqual "parseUInt" Nothing                       (runParser parseUInt "-42foobar")),
        TestCase (assertEqual "parseUInt" Nothing                       (runParser parseUInt "+42foobar")),
        TestCase (assertEqual "parseUInt" Nothing                       (runParser parseUInt "foobar42"))
    ]

parseIntTest :: Test
parseIntTest =
    TestList
    [
        TestCase (assertEqual "parseInt" (Just (1234, ""))              (runParser parseInt "1234")),
        TestCase (assertEqual "parseInt" (Just (1234, "foobar"))        (runParser parseInt "1234foobar")),
        TestCase (assertEqual "parseInt" Nothing                        (runParser parseInt "foobar")),
        TestCase (assertEqual "parseInt" (Just (0, ""))                 (runParser parseInt "0")),
        TestCase (assertEqual "parseInt" (Just (0, ""))                 (runParser parseInt "00")),
        TestCase (assertEqual "parseInt" (Just (1, ""))                 (runParser parseInt "01")),
        TestCase (assertEqual "parseInt" (Just (1, ""))                 (runParser parseInt "0001")),
        TestCase (assertEqual "parseInt" (Just (0, ""))                 (runParser parseInt "0000")),
        TestCase (assertEqual "parseInt" (Just (42, "foobar"))          (runParser parseInt "42foobar")),
        TestCase (assertEqual "parseInt" (Just (-42, "foobar"))         (runParser parseInt "-42foobar")),
        TestCase (assertEqual "parseInt" (Just (42, "foobar"))          (runParser parseInt "+42foobar")),
        TestCase (assertEqual "parseInt" Nothing                        (runParser parseInt "foobar42"))
    ]

parseEscapedCharTest :: Test
parseEscapedCharTest =
    TestList
    [
        TestCase (assertEqual "parseEscapedChar" (Just ('\\', " foobar")) (runParser parseEscapedChar "\\\\ foobar")),
        TestCase (assertEqual "parseEscapedChar" (Just ('\"', " foobar")) (runParser parseEscapedChar "\\\" foobar")),
        TestCase (assertEqual "parseEscapedChar" (Just ('\'', " foobar")) (runParser parseEscapedChar "\\\' foobar")),
        TestCase (assertEqual "parseEscapedChar" (Just ('\n', " foobar")) (runParser parseEscapedChar "\\n foobar")),
        TestCase (assertEqual "parseEscapedChar" (Just ('\t', " foobar")) (runParser parseEscapedChar "\\t foobar")),
        TestCase (assertEqual "parseEscapedChar" (Just ('\b', " foobar")) (runParser parseEscapedChar "\\b foobar")),
        TestCase (assertEqual "parseEscapedChar" (Just ('\r', " foobar")) (runParser parseEscapedChar "\\r foobar")),
        TestCase (assertEqual "parseEscapedChar" (Just ('\f', " foobar")) (runParser parseEscapedChar "\\f foobar")),
        TestCase (assertEqual "parseEscapedChar" (Just ('\v', " foobar")) (runParser parseEscapedChar "\\v foobar")),
        TestCase (assertEqual "parseEscapedChar" (Just ('😂', " foobar")) (runParser parseEscapedChar "\\128514 foobar")),
        TestCase (assertEqual "parseEscapedChar" Nothing                  (runParser parseEscapedChar "v foobar")),
        TestCase (assertEqual "parseEscapedChar" Nothing                  (runParser parseEscapedChar "\\u foobar")),
        TestCase (assertEqual "parseEscapedChar" Nothing                  (runParser parseEscapedChar "foobar"))
    ]

parsePairTest :: Test
parsePairTest =
    TestList
    [
        TestCase (assertEqual "parsePair" (Just ((1, 2), ""))           (runParser (parsePair parseInt) "(1 2)")),
        TestCase (assertEqual "parsePair" Nothing                       (runParser (parsePair parseInt) "(1 2 3)")),
        TestCase (assertEqual "parsePair" Nothing                       (runParser (parsePair parseInt) "(1)")),
        TestCase (assertEqual "parsePair" Nothing                       (runParser (parsePair parseInt) "(1")),
        TestCase (assertEqual "parsePair" Nothing                       (runParser (parsePair parseInt) "1 2)")),
        TestCase (assertEqual "parsePair" Nothing                       (runParser (parsePair parseInt) "1 2")),
        TestCase (assertEqual "parsePair" Nothing                       (runParser (parsePair parseInt) "1")),
        TestCase (assertEqual "parsePair" Nothing                       (runParser (parsePair parseInt) ""))
    ]

parseOrBothTest :: Test
parseOrBothTest =
    TestList
    [
        TestCase (assertEqual "parseOrBoth" (Just (Left '4', "2foobar"))    (runParser (parseOrBoth (parseChar '4') parseInt) "42foobar")),
        TestCase (assertEqual "parseOrBoth" (Just (Left 42, "foobar"))      (runParser (parseOrBoth parseInt (parseChar '4')) "42foobar")),
        TestCase (assertEqual "parseOrBoth" (Just (Left 42, "foobar"))      (runParser (parseOrBoth parseInt (parseChar 'a')) "42foobar")),
        TestCase (assertEqual "parseOrBoth" (Just (Right '4', "2foobar"))   (runParser (parseOrBoth (parseChar 'a') (parseChar '4')) "42foobar")),
        TestCase (assertEqual "parseOrBoth" Nothing                         (runParser (parseOrBoth (parseChar 'a') (parseChar 'b')) "42foobar"))
    ]

parseListTest :: Test
parseListTest =
    TestList
    [
        TestCase (assertEqual "parseList" (Just ([1, 2], ""))                               (runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "(1 2)")),
        TestCase (assertEqual "parseList" (Just ([1, 2, 3], "alut"))                        (runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "(1 2 3)alut")),
        TestCase (assertEqual "parseList" (Just ([1, 2, 3], ""))                            (runParser (parseList (parseChar '(') (parseChar ',') (parseChar ')') (parseChar ' ') parseInt) "(1,  2,  3)")),
        TestCase (assertEqual "parseList" Nothing                                           (runParser (parseList (parseChar '(') (parseChar ',') (parseChar ')') (parseChar ' ') parseInt) "alut(1,2,3)")),
        TestCase (assertEqual "parseList" (Just ([1], ""))                                  (runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "(1)")),
        TestCase (assertEqual "parseList" Nothing                                           (runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "(1")),
        TestCase (assertEqual "parseList" Nothing                                           (runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "1 2)")),
        TestCase (assertEqual "parseList" Nothing                                           (runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "1 2")),
        TestCase (assertEqual "parseList" Nothing                                           (runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "1")),
        TestCase (assertEqual "parseList" Nothing                                           (runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "")),
        TestCase (assertEqual "parseList" (Just ([1, 2, 3, 4, 5], ""))                      (runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "(1 2 3 4 5)")),
        TestCase (assertEqual "parseList" (Just ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11], ""))  (runParser (parseList (parseChar '(') (parseChar ' ') (parseChar ')') (parseChar ' ') parseInt) "(1 2 3 4 5 6 7 8 9 10 11)"))
    ]

showFileTest :: Test
showFileTest =
    TestList
    [
        TestCase (assertEqual "showFile" "Hello World\n" (show (File ["Hello World"]))),
        TestCase (assertEqual "showFile" "Hello World\n42\n" (show (File ["Hello World", "42"]))),
        TestCase (assertEqual "showFile" "Hello World\n42\n(define my_var 42)\n" (show (File ["Hello World", "42", "(define my_var 42)"])))
    ]

showTokenTest :: Test
showTokenTest =
    TestList
    [
        TestCase (assertEqual "showToken" "OpenPARENTHESIS" (show (OpenParenthesis))),
        TestCase (assertEqual "showToken" "ClosePARENTHESIS" (show (CloseParenthesis))),
        TestCase (assertEqual "showToken" "SPACE" (show (SpaceToken))),
        TestCase (assertEqual "showToken" "IF" (show (IfToken))),
        TestCase (assertEqual "showToken" "ELSEIF" (show (ElseIfToken))),
        TestCase (assertEqual "showToken" "ELSE" (show (ElseToken))),
        TestCase (assertEqual "showToken" "FOR" (show (ForToken))),
        TestCase (assertEqual "showToken" "WHILE" (show (WhileToken))),
        TestCase (assertEqual "showToken" "FUN" (show (FunToken))),
        TestCase (assertEqual "showToken" "FUNTYPE" (show (FunTypeToken))),
        TestCase (assertEqual "showToken" "INT" (show (IntTypeToken))),
        TestCase (assertEqual "showToken" "FLOAT" (show (FloatTypeToken))),
        TestCase (assertEqual "showToken" "CHAR" (show (CharTypeToken))),
        TestCase (assertEqual "showToken" "STRING" (show (StringTypeToken))),
        TestCase (assertEqual "showToken" "OpenBRACKET" (show (OpenBracket))),
        TestCase (assertEqual "showToken" "CloseBRACKET" (show (CloseBracket))),
        TestCase (assertEqual "showToken" "OpenBRACES" (show (OpenBraces))),
        TestCase (assertEqual "showToken" "CloseBRACES" (show (CloseBraces))),
        -- TestCase (assertEqual "showToken" "/*" (show (CommentStart))),
        -- TestCase (assertEqual "showToken" "*/" (show (CommentEnd))),
        -- TestCase (assertEqual "showToken" "//" (show (InlineCommentStart))),
        TestCase (assertEqual "showToken" "DEFINE" (show (DefineToken))),
        TestCase (assertEqual "showToken" "INCLUDE" (show (IncludeToken))),
        TestCase (assertEqual "showToken" "42" (show (IntToken 42))),
        TestCase (assertEqual "showToken" "42.42" (show (FloatToken 42.42))),
        TestCase (assertEqual "showToken" "\"Hello World\"" (show (SymbolToken "\"Hello World\""))),
        TestCase (assertEqual "showToken" "\"Hello World\"" (show (StringToken "Hello World"))),
        TestCase (assertEqual "showToken" "'c'" (show (CharToken 'c'))),
        TestCase (assertEqual "showToken" "COMMA" (show (CommaToken))),
        TestCase (assertEqual "showToken" "RETURN" (show (ReturnToken))),
        TestCase (assertEqual "showToken" "LineSEPARATOR" (show (LineSeparator))),
        TestCase (assertEqual "showToken" "[1,2,3]" (show (ListToken [IntToken 1, IntToken 2, IntToken 3]))),

        TestCase (assertEqual "showToken" "Assign" (show (AssignToken))),
        TestCase (assertEqual "showToken" "Increment" (show (IncrementToken))),
        TestCase (assertEqual "showToken" "Decrement" (show (DecrementToken))),
        TestCase (assertEqual "showToken" "PlusEqual" (show (PlusEqualToken))),
        TestCase (assertEqual "showToken" "MinusEqual" (show (MinusEqualToken))),
        TestCase (assertEqual "showToken" "TimesEqual" (show (TimesEqualToken))),
        TestCase (assertEqual "showToken" "DivideEqual" (show (DivideEqualToken))),
        TestCase (assertEqual "showToken" "ModuloEqual" (show (ModuloEqualToken))),
        TestCase (assertEqual "showToken" "Plus" (show (PlusToken))),
        TestCase (assertEqual "showToken" "Minus" (show (MinusToken))),
        TestCase (assertEqual "showToken" "Times" (show (TimesToken))),
        TestCase (assertEqual "showToken" "Divide" (show (DivideToken))),
        TestCase (assertEqual "showToken" "Modulo" (show (ModuloToken))),
        TestCase (assertEqual "showToken" "And" (show (AndToken))),
        TestCase (assertEqual "showToken" "Or" (show (OrToken))),
        TestCase (assertEqual "showToken" "Not" (show (NotToken))),
        TestCase (assertEqual "showToken" "Equal" (show (EqualToken))),
        TestCase (assertEqual "showToken" "NotEqual" (show (NotEqualToken))),
        TestCase (assertEqual "showToken" "LessThan" (show (LessThanToken))),
        TestCase (assertEqual "showToken" "LessThanEqual" (show (LessThanEqualToken))),
        TestCase (assertEqual "showToken" "GreaterThan" (show (GreaterThanToken))),
        TestCase (assertEqual "showToken" "GreaterThanEqual" (show (GreaterThanEqualToken)))
    ]

equalsTokenTest :: Test
equalsTokenTest =
    TestList
    [
        TestCase (assertEqual "equalsToken" True (OpenParenthesis == OpenParenthesis)),
        TestCase (assertEqual "equalsToken" True (CloseParenthesis == CloseParenthesis)),
        TestCase (assertEqual "equalsToken" True (SpaceToken == SpaceToken)),
        TestCase (assertEqual "equalsToken" True (IfToken == IfToken)),
        TestCase (assertEqual "equalsToken" True (ElseIfToken == ElseIfToken)),
        TestCase (assertEqual "equalsToken" True (ElseToken == ElseToken)),
        TestCase (assertEqual "equalsToken" True (ForToken == ForToken)),
        TestCase (assertEqual "equalsToken" True (WhileToken == WhileToken)),
        TestCase (assertEqual "equalsToken" True (FunToken == FunToken)),
        TestCase (assertEqual "equalsToken" True (FunTypeToken == FunTypeToken)),
        TestCase (assertEqual "equalsToken" True (IntTypeToken == IntTypeToken)),
        TestCase (assertEqual "equalsToken" True (FloatTypeToken == FloatTypeToken)),
        TestCase (assertEqual "equalsToken" True (CharTypeToken == CharTypeToken)),
        TestCase (assertEqual "equalsToken" True (StringTypeToken == StringTypeToken)),
        TestCase (assertEqual "equalsToken" True (OpenBracket == OpenBracket)),
        TestCase (assertEqual "equalsToken" True (CloseBracket == CloseBracket)),
        TestCase (assertEqual "equalsToken" True (OpenBraces == OpenBraces)),
        TestCase (assertEqual "equalsToken" True (CloseBraces == CloseBraces)),
        -- TestCase (assertEqual "equalsToken" True (CommentStart == CommentStart)),
        -- TestCase (assertEqual "equalsToken" True (CommentEnd == CommentEnd)),
        -- TestCase (assertEqual "equalsToken" True (InlineCommentStart == InlineCommentStart)),
        TestCase (assertEqual "equalsToken" True (DefineToken == DefineToken)),
        TestCase (assertEqual "equalsToken" True (IncludeToken == IncludeToken)),
        TestCase (assertEqual "equalsToken" True ((IntToken 42) == (IntToken 42))),
        TestCase (assertEqual "equalsToken" True ((FloatToken 42.42) == (FloatToken 42.42))),
        TestCase (assertEqual "equalsToken" True ((SymbolToken "\"Hello World\"") == (SymbolToken "\"Hello World\""))),
        TestCase (assertEqual "equalsToken" True ((StringToken "Hello World") == (StringToken "Hello World"))),
        TestCase (assertEqual "equalsToken" True ((CharToken 'c') == (CharToken 'c'))),
        TestCase (assertEqual "equalsToken" True (CommaToken == CommaToken)),
        TestCase (assertEqual "equalsToken" True (ReturnToken == ReturnToken)),
        TestCase (assertEqual "equalsToken" True (LineSeparator == LineSeparator)),
        TestCase (assertEqual "equalsToken" True ((ListToken [IntToken 1, IntToken 2, IntToken 3]) == (ListToken [IntToken 1, IntToken 2, IntToken 3]))),

        TestCase (assertEqual "equalsToken" False (OpenParenthesis == CloseParenthesis)),
        TestCase (assertEqual "equalsToken" False (CloseParenthesis == SpaceToken)),
        TestCase (assertEqual "equalsToken" False (SpaceToken == IfToken)),
        TestCase (assertEqual "equalsToken" False (IfToken == ElseIfToken)),
        TestCase (assertEqual "equalsToken" False (ElseIfToken == ElseToken)),
        TestCase (assertEqual "equalsToken" False (ElseToken == ForToken)),
        TestCase (assertEqual "equalsToken" False (ForToken == WhileToken)),
        TestCase (assertEqual "equalsToken" False (WhileToken == FunToken)),
        TestCase (assertEqual "equalsToken" False (FunToken == FunTypeToken)),
        TestCase (assertEqual "equalsToken" False (FunTypeToken == IntTypeToken)),
        TestCase (assertEqual "equalsToken" False (IntTypeToken == CharTypeToken)),
        TestCase (assertEqual "equalsToken" False (CharTypeToken == StringTypeToken)),
        TestCase (assertEqual "equalsToken" False (StringTypeToken == OpenBracket)),
        TestCase (assertEqual "equalsToken" False (OpenBracket == CloseBracket)),
        TestCase (assertEqual "equalsToken" False (CloseBracket == OpenBraces)),
        TestCase (assertEqual "equalsToken" False (OpenBraces == CloseBraces))
        -- TestCase (assertEqual "equalsToken" False (CloseBraces == CommentStart)),
        -- TestCase (assertEqual "equalsToken" False (CommentStart == CommentEnd))
    ]

equalsASTTest :: Test
equalsASTTest =
    TestList
    [
        TestCase (assertEqual "equalsAST" True (AST [] == AST [])),
        TestCase (assertEqual "equalsAST" True (AST [DeadLeafAST] == AST [DeadLeafAST])),
        TestCase (assertEqual "equalsAST" True (AST [IntAST 42] == AST [IntAST 42])),
        TestCase (assertEqual "equalsAST" True (AST [SymbolAST "\"Hello World\""] == AST [SymbolAST "\"Hello World\""])),
        TestCase (assertEqual "equalsAST" True (AST [CharAST 'c'] == AST [CharAST 'c'])),
        TestCase (assertEqual "equalsAST" True (AST [AST [IntAST 1, IntAST 2, IntAST 3]] == AST [AST [IntAST 1, IntAST 2, IntAST 3]])),
        TestCase (assertEqual "equalsAST" True (AST [DefineAST "my_var" (AST [IntAST 42])] == AST [DefineAST "my_var" (AST [IntAST 42])])),
        TestCase (assertEqual "equalsAST" True (AST [IfAST (AST [IntAST 1, SymbolAST "<", IntAST 2]) (AST [IntAST 41]) (AST [ElseIfAST (AST [IntAST 1, SymbolAST ">", IntAST 2]) (AST [IntAST 42]) (AST [ElseAST (AST [IntAST 43])])])] == AST [IfAST (AST [IntAST 1, SymbolAST "<", IntAST 2]) (AST [IntAST 41]) (AST [ElseIfAST (AST [IntAST 1, SymbolAST ">", IntAST 2]) (AST [IntAST 42]) (AST [ElseAST (AST [IntAST 43])])])])),
        TestCase (assertEqual "equalsAST" True (AST [ForAST (AST [IntTypeAST,SymbolAST "i",SymbolAST "=",IntAST 0]) (AST [SymbolAST "i",SymbolAST "<",IntAST 10]) (AST [SymbolAST "i++"]) (AST [AST [SymbolAST "a",SymbolAST "=",SymbolAST "a",SymbolAST "+",IntAST 1]])] == AST [ForAST (AST [IntTypeAST,SymbolAST "i",SymbolAST "=",IntAST 0]) (AST [SymbolAST "i",SymbolAST "<",IntAST 10]) (AST [SymbolAST "i++"]) (AST [AST [SymbolAST "a",SymbolAST "=",SymbolAST "a",SymbolAST "+",IntAST 1]])])),
        TestCase (assertEqual "equalsAST" True (AST [WhileAST (AST [SymbolAST "i",SymbolAST "<",IntAST 10]) (AST [AST [SymbolAST "a",SymbolAST "=",SymbolAST "a",SymbolAST "+",IntAST 1]])] == AST [WhileAST (AST [SymbolAST "i",SymbolAST "<",IntAST 10]) (AST [AST [SymbolAST "a",SymbolAST "=",SymbolAST "a",SymbolAST "+",IntAST 1]])])),
        TestCase (assertEqual "equalsAST" True (AST [FunAST "main" (AST [AST [IntTypeAST,SymbolAST "n"],AST [CharTypeAST,SymbolAST "c"]]) (AST [IntTypeAST]) (AST [AST [SymbolAST "return",IntAST 0]])] == AST [FunAST "main" (AST [AST [IntTypeAST,SymbolAST "n"],AST [CharTypeAST,SymbolAST "c"]]) (AST [IntTypeAST]) (AST [AST [SymbolAST "return",IntAST 0]])])),
        TestCase (assertEqual "equalsAST" True (StringAST "Hello World" == StringAST "Hello World")),
        TestCase (assertEqual "equalsAST" True (StringTypeAST == StringTypeAST))
    ]

printASTTest :: Test
printASTTest =
    TestList
    [
        TestCase (assertEqual "printAST" "AST\n" (printAST (AST []))),
        TestCase (assertEqual "printAST" "AST\n|   DefineAST my_var\n|   |   AST\n|   |   |   DeadLeafAST\n" (printAST (AST [DefineAST "my_var" (AST [DeadLeafAST])]))),
        TestCase (assertEqual "printAST" "AST\n|   DefineAST my_var\n|   |   AST\n|   |   |   IntAST 42\n" (printAST (AST [DefineAST "my_var" (AST [IntAST 42])]))),
        TestCase (assertEqual "printAST" "AST\n|   DefineAST my_var\n|   |   AST\n|   |   |   SymbolAST \"Hello World\"\n" (printAST (AST [DefineAST "my_var" (AST [SymbolAST "\"Hello World\""])])))
    ]

parseKeywordTest :: Test
parseKeywordTest =
    TestList
    [
        TestCase (assertEqual "parseKeyword" (Just (IfToken, " (1 2)")) (runParser (parseKeyword "if" IfToken) "if (1 2)")),
        TestCase (assertEqual "parseKeyword" (Just (ElseToken, " if (1 2)")) (runParser (parseKeyword "else" ElseToken) "else if (1 2)")),
        TestCase (assertEqual "parseKeyword" (Just (ElseToken, " (1 2)")) (runParser (parseKeyword "else" ElseToken) "else (1 2)")),
        TestCase (assertEqual "parseKeyword" (Just (ForToken, " (1 2)")) (runParser (parseKeyword "for" ForToken) "for (1 2)")),
        TestCase (assertEqual "parseKeyword" Nothing (runParser (parseKeyword "if" IfToken) "i(1 2)")),
        TestCase (assertEqual "parseKeyword" Nothing (runParser (parseKeyword "else" ElseToken) "elesif(1 2)")),
        TestCase (assertEqual "parseKeyword" Nothing (runParser (parseKeyword "else" ElseToken) "lese"))
    ]

parseIntTokenTest :: Test
parseIntTokenTest =
    TestList
    [
        TestCase (assertEqual "parseIntToken" (Just (IntToken 42, " (1 2)")) (runParser parseIntToken "42 (1 2)")),
        TestCase (assertEqual "parseIntToken" (Just (IntToken 42, "(1 2)")) (runParser parseIntToken "42(1 2)")),
        TestCase (assertEqual "parseIntToken" Nothing (runParser parseIntToken "i42 (1 2)")),
        TestCase (assertEqual "parseIntToken" Nothing (runParser parseIntToken "if (1 2)"))
    ]

parseSymbolTokenTest :: Test
parseSymbolTokenTest =
    TestList
    [
        TestCase (assertEqual "parseSymbolToken" (Just (SymbolToken "H", "ello World (1 2)")) (runParser parseSymbolToken "Hello World (1 2)")),
        TestCase (assertEqual "parseSymbolToken" (Just (SymbolToken "f", "i(1 2)")) (runParser parseSymbolToken "fi(1 2)"))
    ]

parseStringTokenTest :: Test
parseStringTokenTest =
    TestList
    [
        TestCase (assertEqual "parseStringToken" (Just (StringToken "Hello World", " (1 2)")) (runParser parseStringToken "\"Hello World\" (1 2)")),
        TestCase (assertEqual "parseStringToken" (Just (StringToken "Hello World", "(1 2)")) (runParser parseStringToken "\"Hello World\"(1 2)")),
        TestCase (assertEqual "parseStringToken" Nothing (runParser parseStringToken "i\"Hello World\" (1 2)"))
    ]

parseCharTokenTest :: Test
parseCharTokenTest =
    TestList
    [
        TestCase (assertEqual "parseCharToken" (Just (CharToken 'c', " (1 2)")) (runParser parseCharToken "'c' (1 2)")),
        TestCase (assertEqual "parseCharToken" (Just (CharToken 'c', "(1 2)")) (runParser parseCharToken "'c'(1 2)")),
        TestCase (assertEqual "parseCharToken" Nothing (runParser parseCharToken "i'c' (1 2)"))
    ]

parseTokenTest :: Test
parseTokenTest =
    TestList
    [
        TestCase (assertEqual "parseToken" (Just (IntToken 42, " (1 2)")) (runParser parseToken "42 (1 2)")),
        TestCase (assertEqual "parseToken" (Just (FloatToken 42.84, " (1 2)")) (runParser parseToken "42.84 (1 2)")),
        TestCase (assertEqual "parseToken" (Just (SymbolToken "H", "ello World (1 2)")) (runParser parseToken "Hello World (1 2)")),
        TestCase (assertEqual "parseToken" (Just (StringToken "Hello World", " (1 2)")) (runParser parseToken "\"Hello World\" (1 2)")),
        TestCase (assertEqual "parseToken" (Just (CharToken 'c', " (1 2)")) (runParser parseToken "'c' (1 2)")),
        TestCase (assertEqual "parseToken" (Just (OpenParenthesis, " (1 2)")) (runParser parseToken "( (1 2)")),
        TestCase (assertEqual "parseToken" (Just (CloseParenthesis, " (1 2)")) (runParser parseToken ") (1 2)")),
        TestCase (assertEqual "parseToken" (Just (OpenBracket, " (1 2)")) (runParser parseToken "[ (1 2)")),
        TestCase (assertEqual "parseToken" (Just (CloseBracket, " (1 2)")) (runParser parseToken "] (1 2)")),
        TestCase (assertEqual "parseToken" (Just (OpenBraces, " (1 2)")) (runParser parseToken "{ (1 2)")),
        TestCase (assertEqual "parseToken" (Just (CloseBraces, " (1 2)")) (runParser parseToken "} (1 2)")),
        TestCase (assertEqual "parseToken" (Just (CommaToken, " (1 2)")) (runParser parseToken ", (1 2)")),
        TestCase (assertEqual "parseToken" (Just (LineSeparator, " (1 2)")) (runParser parseToken "; (1 2)")),
        TestCase (assertEqual "parseToken" (Just (IfToken, " (1 2)")) (runParser parseToken "if (1 2)")),
        TestCase (assertEqual "parseToken" (Just (ElseToken, " if (1 2)")) (runParser parseToken "else if (1 2)")),
        TestCase (assertEqual "parseToken" (Just (ElseToken, " (1 2)")) (runParser parseToken "else (1 2)")),
        TestCase (assertEqual "parseToken" (Just (ForToken, " (1 2)")) (runParser parseToken "for (1 2)")),
        TestCase (assertEqual "parseToken" (Just (WhileToken, " (1 2)")) (runParser parseToken "while (1 2)")),
        TestCase (assertEqual "parseToken" (Just (FunToken, " (1 2)")) (runParser parseToken "fun (1 2)")),
        TestCase (assertEqual "parseToken" (Just (ReturnToken, " (1 2)")) (runParser parseToken "return (1 2)")),
        TestCase (assertEqual "parseToken" (Just (FunTypeToken, " (1 2)")) (runParser parseToken ": (1 2)")),
        TestCase (assertEqual "parseToken" (Just (IntTypeToken, " (1 2)")) (runParser parseToken "int (1 2)")),
        TestCase (assertEqual "parseToken" (Just (FloatTypeToken, " (1 2)")) (runParser parseToken "float (1 2)")),
        TestCase (assertEqual "parseToken" (Just (CharTypeToken, " (1 2)")) (runParser parseToken "char (1 2)")),
        TestCase (assertEqual "parseToken" (Just (StringTypeToken, " (1 2)")) (runParser parseToken "string (1 2)")),
        -- TestCase (assertEqual "parseToken" (Just (CommentStart, " (1 2)")) (runParser parseToken "/* (1 2)")),
        -- TestCase (assertEqual "parseToken" (Just (CommentEnd, " (1 2)")) (runParser parseToken "*/ (1 2)")),
        -- TestCase (assertEqual "parseToken" (Just (InlineCommentStart, " (1 2)")) (runParser parseToken "// (1 2)")),
        TestCase (assertEqual "parseToken" (Just (DefineToken, " (1 2)")) (runParser parseToken "#define (1 2)")),
        TestCase (assertEqual "parseToken" (Just (IncludeToken, " (1 2)")) (runParser parseToken "#include (1 2)")),
        TestCase (assertEqual "parseToken" (Just (EqualToken, " (1 2)")) (runParser parseToken "== (1 2)")),
        TestCase (assertEqual "parseToken" (Just (NotEqualToken, " (1 2)")) (runParser parseToken "!= (1 2)")),
        TestCase (assertEqual "parseToken" (Just (LessThanToken, " (1 2)")) (runParser parseToken "< (1 2)")),
        TestCase (assertEqual "parseToken" (Just (LessThanEqualToken, " (1 2)")) (runParser parseToken "<= (1 2)")),
        TestCase (assertEqual "parseToken" (Just (GreaterThanToken, " (1 2)")) (runParser parseToken "> (1 2)")),
        TestCase (assertEqual "parseToken" (Just (GreaterThanEqualToken, " (1 2)")) (runParser parseToken ">= (1 2)")),
        TestCase (assertEqual "parseToken" (Just (PlusToken, " (1 2)")) (runParser parseToken "+ (1 2)")),
        TestCase (assertEqual "parseToken" (Just (MinusToken, " (1 2)")) (runParser parseToken "- (1 2)")),
        TestCase (assertEqual "parseToken" (Just (TimesToken, " (1 2)")) (runParser parseToken "* (1 2)")),
        TestCase (assertEqual "parseToken" (Just (DivideToken, " (1 2)")) (runParser parseToken "/ (1 2)")),
        TestCase (assertEqual "parseToken" (Just (ModuloToken, " (1 2)")) (runParser parseToken "% (1 2)")),
        TestCase (assertEqual "parseToken" (Just (AndToken, " (1 2)")) (runParser parseToken "&& (1 2)")),
        TestCase (assertEqual "parseToken" (Just (OrToken, " (1 2)")) (runParser parseToken "|| (1 2)")),
        TestCase (assertEqual "parseToken" (Just (NotToken, " (1 2)")) (runParser parseToken "! (1 2)")),
        TestCase (assertEqual "parseToken" (Just (PlusEqualToken, " (1 2)")) (runParser parseToken "+= (1 2)")),
        TestCase (assertEqual "parseToken" (Just (MinusEqualToken, " (1 2)")) (runParser parseToken "-= (1 2)")),
        TestCase (assertEqual "parseToken" (Just (TimesEqualToken, " (1 2)")) (runParser parseToken "*= (1 2)")),
        TestCase (assertEqual "parseToken" (Just (DivideEqualToken, " (1 2)")) (runParser parseToken "/= (1 2)")),
        TestCase (assertEqual "parseToken" (Just (ModuloEqualToken, " (1 2)")) (runParser parseToken "%= (1 2)")),
        TestCase (assertEqual "parseToken" (Just (IncrementToken, " (1 2)")) (runParser parseToken "++ (1 2)")),
        TestCase (assertEqual "parseToken" (Just (DecrementToken, " (1 2)")) (runParser parseToken "-- (1 2)")),
        TestCase (assertEqual "parseToken" (Just (IntToken 5, " (1 2)")) (runParser parseToken "5 (1 2)")),
        TestCase (assertEqual "parseToken" (Just (FloatToken 5.42, " (1 2)")) (runParser parseToken "5.42 (1 2)")),
        TestCase (assertEqual "parseToken" (Just (SpaceToken, "(1 2)")) (runParser parseToken " (1 2)")),
        TestCase (assertEqual "parseToken" (Just (SpaceToken, "(1 2)")) (runParser parseToken "\t(1 2)"))
    ]

parseLineTest :: Test
parseLineTest =
    TestList
    [
        TestCase (do
            let originalLine = "42"
            result <- parseLine "42" 0 "" originalLine
            assertEqual "parseLine" ([IntToken 42]) (result)
        ),
        TestCase (do
            let originalLine = "Hello World"
            result <- parseLine "Hello World" 0 "" originalLine
            assertEqual "parseLine" ([SymbolToken "H", SymbolToken "e", SymbolToken "l", SymbolToken "l", SymbolToken "o", SpaceToken, SymbolToken "W", SymbolToken "o", SymbolToken "r", SymbolToken "l", SymbolToken "d"]) (result)
        )
    ]

parseFileTest :: Test
parseFileTest =
    TestList
    [
        TestCase (do
            let originalFile = (File ["42"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([IntToken 42]) (result)
        ),
        TestCase (do
            let originalFile = (File ["Hello World"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([SymbolToken "Hello", SymbolToken "World"]) (result)
        ),
        TestCase (do
            let originalFile = (File ["-42", "Hello World"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([IntToken (-42), SymbolToken "Hello", SymbolToken "World"]) (result)
        ),
        TestCase (do
            let originalFile = (File ["-42.84"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([FloatToken (-42.84)]) (result)
        ),
        TestCase (do
            let originalFile = (File ["if (1 < 2)"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([IfToken, OpenParenthesis, IntToken 1, LessThanToken, IntToken 2, CloseParenthesis]) (result)
        ),
        TestCase (do
            let originalFile = (File ["else if (1 > 2)"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([ElseIfToken, OpenParenthesis, IntToken 1, GreaterThanToken, IntToken 2, CloseParenthesis]) (result)
        ),
        TestCase (do
            let originalFile = (File ["else (1 > 2)"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([ElseToken, OpenParenthesis, IntToken 1, GreaterThanToken, IntToken 2, CloseParenthesis]) (result)
        ),
        TestCase (do
            let originalFile = (File ["fun my_fun (int n) : int // similar to C"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([FunToken, SymbolToken "my_fun", OpenParenthesis, IntTypeToken, SymbolToken "n", CloseParenthesis, FunTypeToken, IntTypeToken]) (result)
        ),
        TestCase (do
            let originalFile = (File ["int a = factorial(a, 'a');;; int b = factorial(b, 'b');"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([IntTypeToken, SymbolToken "a", AssignToken, SymbolToken "factorial", OpenParenthesis, SymbolToken "a", CommaToken, CharToken 'a', CloseParenthesis, LineSeparator, IntTypeToken, SymbolToken "b", AssignToken, SymbolToken "factorial", OpenParenthesis, SymbolToken "b", CommaToken, CharToken 'b', CloseParenthesis, LineSeparator]) (result)
        ),
        TestCase (do
            let originalFile = (File ["int int_my_int = 42"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([IntTypeToken, SymbolToken "int_my_int", AssignToken, IntToken 42]) (result)
        ),
        TestCase (do
            let originalFile = (File ["char char_my_char = 'c'"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([CharTypeToken, SymbolToken "char_my_char", AssignToken, CharToken 'c']) (result)
        ),
        TestCase (do
            let originalFile = (File ["string string_my_string = \"Hello World\""])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([StringTypeToken, SymbolToken "string_my_string", AssignToken, StringToken "Hello World"]) (result)
        ),
        TestCase (do
            let originalFile = (File ["int if_my_if = 42"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([IntTypeToken, SymbolToken "if_my_if", AssignToken, IntToken 42]) (result)
        ),
        TestCase (do
            let originalFile = (File ["int else_my_else = 42"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([IntTypeToken, SymbolToken "else_my_else", AssignToken, IntToken 42]) (result)
        ),
        TestCase (do
            let originalFile = (File ["int fun_my_fun = 42"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([IntTypeToken, SymbolToken "fun_my_fun", AssignToken, IntToken 42]) (result)
        ),
        TestCase (do
            let originalFile = (File ["int for_my_for = 42"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([IntTypeToken, SymbolToken "for_my_for", AssignToken, IntToken 42]) (result)
        ),
        TestCase (do
            let originalFile = (File ["int while_my_while = 42"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([IntTypeToken, SymbolToken "while_my_while", AssignToken, IntToken 42]) (result)
        ),
        TestCase (do
            let originalFile = (File ["int return_my_return = 42"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([IntTypeToken, SymbolToken "return_my_return", AssignToken, IntToken 42]) (result)
        ),
        TestCase (do
            let originalFile = (File ["#include \"test.our\""])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([DefineToken, SymbolToken "my_int", IntTypeToken, FunToken, SymbolToken "main", OpenParenthesis, CloseParenthesis, FunTypeToken, SymbolToken "my_int", OpenBraces, ReturnToken, IntToken 0, LineSeparator, CloseBraces]) (result)
        ),
        TestCase (do
            let originalFile = (File ["#include my_int int"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([IncludeToken, SymbolToken "my_int", IntTypeToken]) (result)
        ),
        TestCase (do
            let originalFile = (File ["float float_my_float = 84.42"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([FloatTypeToken, SymbolToken "float_my_float", AssignToken, FloatToken 84.42]) (result)
        ),
        TestCase (do
            let originalFile = (File ["int 42_my_42 = 42"])
            let cleanedFile = cleanFile originalFile False
            result <- parseFile cleanedFile 0 [""] originalFile
            assertEqual "parseFile" ([IntTypeToken, SymbolToken "42_my_42", AssignToken, IntToken 42]) (result)
        )
    ]

tokenListToSexprTest :: Test
tokenListToSexprTest =
    TestList
    [
        TestCase (do
            let originalFile = (File ["42"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let result = tokenListToSexpr $ tokenList
            assertEqual "tokenListToSexpr" [IntToken 42] (result)
        ),
        TestCase (do
            let originalFile = (File ["Hello World"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let result = tokenListToSexpr $ tokenList
            assertEqual "tokenListToSexpr" [SymbolToken "Hello", SymbolToken "World"] (result)
        ),
        TestCase (do
            let originalFile = (File ["Hello World (42)"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let result = tokenListToSexpr $ tokenList
            assertEqual "tokenListToSexpr" [SymbolToken "Hello", SymbolToken "World", ListToken [IntToken 42]] (result)
        ),
        TestCase (do
            let originalFile = (File ["factorial(n-1)"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let result = tokenListToSexpr $ tokenList
            assertEqual "tokenListToSexpr" [SymbolToken "factorial", ListToken [SymbolToken "n", MinusToken, IntToken 1]] (result)
        ),
        TestCase (do
            let originalFile = (File ["factorial(n-1.42)"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let result = tokenListToSexpr $ tokenList
            assertEqual "tokenListToSexpr" [SymbolToken "factorial", ListToken [SymbolToken "n", MinusToken, FloatToken 1.42]] (result)
        ),
        TestCase (do
            let originalFile = (File ["factorial(n 1)"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let result = tokenListToSexpr $ tokenList
            assertEqual "tokenListToSexpr" [SymbolToken "factorial", ListToken [SymbolToken "n", IntToken 1]] (result)
        ),
        TestCase (do
            let originalFile = (File ["factorial(n 1.42)"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let result = tokenListToSexpr $ tokenList
            assertEqual "tokenListToSexpr" [SymbolToken "factorial", ListToken [SymbolToken "n", FloatToken 1.42]] (result)
        ),
        TestCase (do
            let originalFile = (File ["fun factorial (int n, char c) /* lol this is a comment to try to break something */: (const my_int) // and this is an inline comment"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let result = tokenListToSexpr $ tokenList
            assertEqual "tokenListToSexpr" [FunToken, SymbolToken "factorial", ListToken [IntTypeToken, SymbolToken "n", CommaToken, CharTypeToken, SymbolToken "c"], ListToken [FunTypeToken, ListToken [SymbolToken "const", SymbolToken "my_int"]]] (result)
        ),
        TestCase (do
            let originalFile = (File ["fun factorial ((int n), (char c)) /* lol this is a comment to try to break something */: (const my_int) // and this is an inline comment", "{", "int x[2] = {1, 2}" ,"}"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let result = tokenListToSexpr $ tokenList
            assertEqual "tokenListToSexpr" [FunToken, SymbolToken "factorial", ListToken [ListToken [IntTypeToken, SymbolToken "n"], CommaToken, ListToken [CharTypeToken, SymbolToken "c"]], ListToken [FunTypeToken, ListToken [SymbolToken "const", SymbolToken "my_int"]], ListToken [IntTypeToken, SymbolToken "x", ListToken [IntToken 2], AssignToken, ListToken [IntToken 1, CommaToken, IntToken 2]]] (result)
        )
    ]

splitAtValueTest :: Test
splitAtValueTest =
    TestList
    [
        TestCase (assertEqual "splitAtValue" (Just ("Hello", ' ', "World")) (splitAtValue ' ' "Hello World")),
        TestCase (assertEqual "splitAtValue" (Just ("Hello", ' ', "World Me!")) (splitAtValue ' ' "Hello World Me!")),
        TestCase (assertEqual "splitAtValue" (Nothing) (splitAtValue ' ' "Hello_World"))
    ]

splitAtLastValueTest :: Test
splitAtLastValueTest =
    TestList
    [
        TestCase (assertEqual "splitAtLastValue" (Just ("Hello", ' ', "World")) (splitAtLastValue ' ' "Hello World")),
        TestCase (assertEqual "splitAtLastValue" (Just ("Hello World", ' ', "Me!")) (splitAtLastValue ' ' "Hello World Me!")),
        TestCase (assertEqual "splitAtLastValue" (Nothing) (splitAtLastValue ' ' "Hello_World"))
    ]

getIfChainTest :: Test
getIfChainTest =
    TestList
    [
        TestCase (do
            let originalFile = (File ["else if (1 > 2) { a = 84; } else { a = 168; }"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = getIfChain sexpr
            let expected = ([ElseIfToken, ListToken [IntToken 1, GreaterThanToken, IntToken 2], ListToken [SymbolToken "a", AssignToken, IntToken 84, LineSeparator], ElseToken, ListToken [SymbolToken "a", AssignToken, IntToken 168, LineSeparator]], [])
            assertEqual "getIfChain" (expected) (result)),
        TestCase (do
            let originalFile = (File ["else if (1 > 2) { a = 84; } else if (1 == 2) { a = 168; }"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = getIfChain sexpr
            let expected = ([ElseIfToken, ListToken [IntToken 1, GreaterThanToken, IntToken 2], ListToken [SymbolToken "a", AssignToken, IntToken 84, LineSeparator], ElseIfToken, ListToken [IntToken 1, EqualToken, IntToken 2], ListToken [SymbolToken "a", AssignToken, IntToken 168, LineSeparator]], [])
            assertEqual "getIfChain" (expected) (result)),
        TestCase (do
            let originalFile = (File ["else if (1 > 2) { a = 84; } if (1 == 2) { a = 168; }"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = getIfChain sexpr
            let expected = ([ElseIfToken, ListToken [IntToken 1, GreaterThanToken, IntToken 2], ListToken [SymbolToken "a", AssignToken, IntToken 84, LineSeparator]], [IfToken, ListToken [IntToken 1, EqualToken, IntToken 2], ListToken [SymbolToken "a", AssignToken, IntToken 168, LineSeparator]])
            assertEqual "getIfChain" (expected) (result)),
        TestCase (do
            let originalFile = (File ["else if (1 > 2) { a = 84; } else { a = 168; } print(a);"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = getIfChain sexpr
            let expected = ([ElseIfToken, ListToken [IntToken 1, GreaterThanToken, IntToken 2], ListToken [SymbolToken "a", AssignToken, IntToken 84, LineSeparator], ElseToken, ListToken [SymbolToken "a", AssignToken, IntToken 168, LineSeparator]], [SymbolToken "print", ListToken [SymbolToken "a"], LineSeparator])
            assertEqual "getIfChain" (expected) (result))
    ]

binaryOperatorsASTTest :: Test
binaryOperatorsASTTest =
    TestList
    [
        TestCase (do
            let originalFile = (File ["1 + 2"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = binaryOperatorsAST PlusToken PlusAST sexpr
            let expected = (PlusAST (AST [IntAST 1]) (AST [IntAST 2]))
            assertEqual "binaryOperatorsAST" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 - 2"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = binaryOperatorsAST PlusToken PlusAST sexpr
            let expected = (PlusAST DeadLeafAST DeadLeafAST)
            assertEqual "binaryOperatorsAST" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2 + 3"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = binaryOperatorsAST PlusToken PlusAST sexpr
            let expected = (PlusAST (AST [IntAST 1]) (PlusAST (AST [IntAST 2]) (AST [IntAST 3])))
            assertEqual "binaryOperatorsAST" (expected) (result))
    ]

operatorsAfterASTTest :: Test
operatorsAfterASTTest =
    TestList
    [
        TestCase (do
            let originalFile = (File ["!1"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = operatorsAfterAST NotToken NotAST sexpr
            let expected = (NotAST (AST [IntAST 1]))
            assertEqual "operatorsAfterAST" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = operatorsAfterAST NotToken NotAST sexpr
            let expected = (NotAST DeadLeafAST)
            assertEqual "operatorsAfterAST" (expected) (result))
    ]

operatorsBeforeASTTest :: Test
operatorsBeforeASTTest =
    TestList
    [
        TestCase (do
            let originalFile = (File ["i++"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = operatorsBeforeAST IncrementToken IncrementAST sexpr
            let expected = (IncrementAST (AST [SymbolAST "i"]))
            assertEqual "operatorsBeforeAST" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = operatorsBeforeAST IncrementToken IncrementAST sexpr
            let expected = (IncrementAST DeadLeafAST)
            assertEqual "operatorsBeforeAST" (expected) (result))
    ]

listOperatorsASTCheckTest :: Test
listOperatorsASTCheckTest =
    TestList
    [
        TestCase (do
            let originalFile = (File ["1 + 2"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = listOperatorsASTCheck [PlusToken] sexpr
            let expected = True
            assertEqual "listOperatorsASTCheck" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2 + 3"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = listOperatorsASTCheck [PlusToken] sexpr
            let expected = True
            assertEqual "listOperatorsASTCheck" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2 + 3 - 4"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = listOperatorsASTCheck [MinusToken] sexpr
            let expected = True
            assertEqual "listOperatorsASTCheck" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2 + 3 - 4 / 5"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = listOperatorsASTCheck [MinusToken, DivideToken] sexpr
            let expected = True
            assertEqual "listOperatorsASTCheck" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2 + 3 - 4 * 5"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = listOperatorsASTCheck [MinusToken, DivideToken] sexpr
            let expected = False
            assertEqual "listOperatorsASTCheck" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2 + 3 - 4 / 5"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = listOperatorsASTCheck [TimesToken] sexpr
            let expected = False
            assertEqual "listOperatorsASTCheck" (expected) (result))
    ]

pemdasTreeTest :: Test
pemdasTreeTest =
    TestList
    [
        TestCase (do
            let originalFile = (File [""])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = pemdasTree sexpr
            let expected = (DeadLeafAST)
            assertEqual "pemdasTree" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = pemdasTree sexpr
            let expected =
                    (PlusAST
                        (AST [IntAST 1])
                        (AST [IntAST 2]))
            assertEqual "pemdasTree" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 - 2"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = pemdasTree sexpr
            let expected =
                    (MinusAST
                        (AST [IntAST 1])
                        (AST [IntAST 2]))
            assertEqual "pemdasTree" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 * 2"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = pemdasTree sexpr
            let expected =
                    (TimesAST
                        (AST [IntAST 1])
                        (AST [IntAST 2]))
            assertEqual "pemdasTree" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 / 2"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = pemdasTree sexpr
            let expected =
                    (DivideAST
                        (AST [IntAST 1])
                        (AST [IntAST 2]))
            assertEqual "pemdasTree" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 % 2"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = pemdasTree sexpr
            let expected =
                    (ModuloAST
                        (AST [IntAST 1])
                        (AST [IntAST 2]))
            assertEqual "pemdasTree" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2 - 3"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = pemdasTree sexpr
            let expected =
                    (MinusAST
                        (PlusAST
                            (AST [IntAST 1])
                            (AST [IntAST 2]))
                        (AST [IntAST 3]))
            assertEqual "pemdasTree" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2 - 3 * 4"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = pemdasTree sexpr
            let expected =
                    (MinusAST
                        (PlusAST
                            (AST [IntAST 1])
                            (AST [IntAST 2]))
                        (TimesAST
                            (AST [IntAST 3])
                            (AST [IntAST 4])))
            assertEqual "pemdasTree" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2 - 3 * 4 / 5"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = pemdasTree sexpr
            let expected =
                    (MinusAST
                        (PlusAST
                            (AST [IntAST 1])
                            (AST [IntAST 2]))
                        (DivideAST
                            (TimesAST
                                (AST [IntAST 3])
                                (AST [IntAST 4]))
                            (AST [IntAST 5])))
            assertEqual "pemdasTree" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2 - 3 % 4 * 5 / 6"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = pemdasTree sexpr
            let expected =
                    (MinusAST
                        (PlusAST
                            (AST [IntAST 1])
                            (AST [IntAST 2]))
                        (DivideAST
                            (TimesAST
                                (ModuloAST
                                    (AST [IntAST 3])
                                    (AST [IntAST 4]))
                                (AST [IntAST 5]))
                            (AST [IntAST 6])))
            assertEqual "pemdasTree" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2 - 3 % 4 / 5 * 6"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = pemdasTree sexpr
            let expected =
                    (MinusAST
                        (PlusAST
                            (AST [IntAST 1])
                            (AST [IntAST 2]))
                        (TimesAST
                            (DivideAST
                                (ModuloAST
                                    (AST [IntAST 3])
                                    (AST [IntAST 4]))
                                (AST [IntAST 5]))
                            (AST [IntAST 6])))
            assertEqual "pemdasTree" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2 - 3 * 4 % 5 / 6"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = pemdasTree sexpr
            let expected =
                    (MinusAST
                        (PlusAST
                            (AST [IntAST 1])
                            (AST [IntAST 2]))
                        (DivideAST
                            (ModuloAST
                                (TimesAST
                                    (AST [IntAST 3])
                                    (AST [IntAST 4]))
                                (AST [IntAST 5]))
                            (AST [IntAST 6])))
            assertEqual "pemdasTree" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2 - 3 / 4 * 5 % 6"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = pemdasTree sexpr
            let expected =
                    (MinusAST
                        (PlusAST
                            (AST [IntAST 1])
                            (AST [IntAST 2]))
                        (ModuloAST
                            (TimesAST
                                (DivideAST
                                    (AST [IntAST 3])
                                    (AST [IntAST 4]))
                                (AST [IntAST 5]))
                            (AST [IntAST 6])))
            assertEqual "pemdasTree" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2 - 3 / 4 % 5 * 6"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = pemdasTree sexpr
            let expected =
                    (MinusAST
                        (PlusAST
                            (AST [IntAST 1])
                            (AST [IntAST 2]))
                        (TimesAST
                            (ModuloAST
                                (DivideAST
                                    (AST [IntAST 3])
                                    (AST [IntAST 4]))
                                (AST [IntAST 5]))
                            (AST [IntAST 6])))
            assertEqual "pemdasTree" (expected) (result)),
        TestCase (do
            let originalFile = (File ["1 + 2 - 3 * 4 / 5 % 6"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = pemdasTree sexpr
            let expected =
                    (MinusAST
                        (PlusAST
                            (AST [IntAST 1])
                            (AST [IntAST 2]))
                        (ModuloAST
                            (DivideAST
                                (TimesAST
                                    (AST [IntAST 3])
                                    (AST [IntAST 4]))
                                (AST [IntAST 5]))
                            (AST [IntAST 6])))
            assertEqual "pemdasTree" (expected) (result)),
        TestCase (do
            let originalFile = (File ["(((20 + 5 * 3) - (7 % 3)) / 2 + (15 - 3 * 2) % 4) * ((12 / 6 + 3) - (2 * 4) % 5) + (((9 * 3) - (7 + 2)) / (4 % 3)) - ((18 / 2) + (5 * 2) % 3) + ((10 - 3) * (6 + 2 % 4)) / ((16 - 3 * 2) + (5 / 2)) % 7"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = pemdasTree sexpr -- 48.48
            let expected =
                    (PlusAST -- 48.48
                        (MinusAST -- 44
                            (PlusAST -- 54
                                (TimesAST -- 36
                                    (AST [PlusAST -- 18
                                        (DivideAST -- 17
                                            (AST [MinusAST -- 34
                                                (AST [PlusAST -- 35
                                                    (AST [IntAST 20])
                                                    (TimesAST -- 15
                                                        (AST [IntAST 5])
                                                        (AST [IntAST 3]))])
                                                (AST [ModuloAST -- 1
                                                    (AST [IntAST 7])
                                                    (AST [IntAST 3])])])
                                            (AST [IntAST 2]))
                                        (ModuloAST -- 1
                                            (AST [MinusAST -- 9
                                                (AST [IntAST 15])
                                                (TimesAST -- 6
                                                    (AST [IntAST 3])
                                                    (AST [IntAST 2]))])
                                            (AST [IntAST 4]))])
                                    (AST [MinusAST -- 2
                                        (AST [PlusAST -- 5
                                            (DivideAST -- 2
                                                (AST [IntAST 12])
                                                (AST [IntAST 6]))
                                            (AST [IntAST 3])])
                                        (ModuloAST -- 3
                                            (AST [TimesAST -- 8
                                                (AST [IntAST 2])
                                                (AST [IntAST 4])])
                                            (AST [IntAST 5]))]))
                                (AST [DivideAST -- 18
                                    (AST [MinusAST -- 18
                                        (AST [TimesAST -- 27
                                            (AST [IntAST 9])
                                            (AST [IntAST 3])])
                                        (AST [PlusAST -- 9
                                            (AST [IntAST 7])
                                            (AST [IntAST 2])])])
                                    (AST [ModuloAST -- 1
                                        (AST [IntAST 4])
                                        (AST [IntAST 3])])]))
                            (AST [PlusAST -- 10
                                (AST [DivideAST -- 9
                                    (AST [IntAST 18])
                                    (AST [IntAST 2])])
                                (ModuloAST -- 1
                                    (AST [TimesAST -- 10
                                        (AST [IntAST 5])
                                        (AST [IntAST 2])])
                                    (AST [IntAST 3]))]))
                        (ModuloAST -- 4.48
                            (DivideAST -- 4.48
                                (AST [TimesAST -- 56
                                    (AST [MinusAST -- 7
                                        (AST [IntAST 10])
                                        (AST [IntAST 3])])
                                    (AST [PlusAST -- 8
                                        (AST [IntAST 6])
                                        (ModuloAST -- 2
                                            (AST [IntAST 2])
                                            (AST [IntAST 4]))])])
                                (AST [PlusAST -- 12.5
                                    (AST [MinusAST -- 10
                                        (AST [IntAST 16])
                                        (TimesAST -- 6
                                            (AST [IntAST 3])
                                            (AST [IntAST 2]))])
                                    (AST [DivideAST -- 2.5
                                        (AST [IntAST 5])
                                        (AST [IntAST 2])])]))
                            (AST [IntAST 7])))
            assertEqual "pemdasTree" (expected) (result))
    ]

sexprToAstTest :: Test
sexprToAstTest =
    TestList
    [
        TestCase (do
            let originalFile = (File ["if (i == 1) { a = 42; }"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = sexprToAst sexpr
            let expected =
                    (AST [IfAST
                        (EqualAST (AST [SymbolAST "i"]) (AST [IntAST 1]))
                        (AST [AssignAST (AST [SymbolAST "a"]) (AST [IntAST 42])])
                        (DeadLeafAST)
                    ])
            assertEqual "sexprToAst" (expected) (result)),
        TestCase (do
            let originalFile = (File ["if (i == 1) { a = 42; } else { a = 84; }"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = sexprToAst sexpr
            let expected =
                    (AST [IfAST
                        (EqualAST (AST [SymbolAST "i"]) (AST [IntAST 1]))
                        (AST [AssignAST (AST [SymbolAST "a"]) (AST [IntAST 42])])
                        (AST [ElseAST (AST [AssignAST (AST [SymbolAST "a"]) (AST [IntAST 84])])
                        ])
                    ])
            assertEqual "sexprToAst" (expected) (result)),
        TestCase (do
            let originalFile = (File ["if (i == 1) { a = 42; } else if (i == 2) { a = 84; } else if (i == 3) { a = 168; } else { a = 336; }"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = sexprToAst sexpr
            let expected =
                    (AST [IfAST
                        (EqualAST (AST [SymbolAST "i"]) (AST [IntAST 1]))
                        (AST [AssignAST (AST [SymbolAST "a"]) (AST [IntAST 42])])
                        (AST [ElseIfAST
                            (EqualAST (AST [SymbolAST "i"]) (AST [IntAST 2]))
                            (AST [AssignAST (AST [SymbolAST "a"]) (AST [IntAST 84])])
                            (AST [ElseIfAST
                                (EqualAST (AST [SymbolAST "i"]) (AST [IntAST 3]))
                                (AST [AssignAST (AST [SymbolAST "a"]) (AST [IntAST 168])])
                                (AST [ElseAST
                                    (AST [AssignAST (AST [SymbolAST "a"]) (AST [IntAST 336])])
                                ])
                            ])
                        ])
                    ])
            assertEqual "sexprToAst" (expected) (result)),
        TestCase (do
            let originalFile = (File ["if (1 > 2) { a = 42; } else if (1 < 2) { a = 84; } else { a = 168; } return a;"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = sexprToAst sexpr
            let expected =
                    (AST [IfAST
                        (GreaterThanAST (AST [IntAST 1]) (AST [IntAST 2]))
                        (AST [AssignAST (AST [SymbolAST "a"]) (AST [IntAST 42])])
                        (AST [ElseIfAST
                            (LessThanAST (AST [IntAST 1]) (AST [IntAST 2]))
                            (AST [AssignAST (AST [SymbolAST "a"]) (AST [IntAST 84])])
                            (AST [ElseAST
                                (AST [AssignAST (AST [SymbolAST "a"]) (AST [IntAST 168])])
                            ])
                        ])
                    , ReturnAST (AST [SymbolAST "a"])])
            assertEqual "sexprToAst" (expected) (result)),
        TestCase (do
            let originalFile = (File ["if (!1 >= 2 && 1 == 2) { a = 42; } else if (1 <= 2 || 1 != 2) { a = 84; }"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = sexprToAst sexpr
            let expected =
                    (AST [IfAST
                        (AndAST (NotAST (GreaterThanEqualAST (AST [IntAST 1]) (AST [IntAST 2]))) (EqualAST (AST [IntAST 1]) (AST [IntAST 2])))
                        (AST [AssignAST (AST [SymbolAST "a"]) (AST [IntAST 42])])
                        (AST [ElseIfAST
                            (OrAST (LessThanEqualAST (AST [IntAST 1]) (AST [IntAST 2])) (NotEqualAST (AST [IntAST 1]) (AST [IntAST 2])))
                            (AST [AssignAST (AST [SymbolAST "a"]) (AST [IntAST 84])])
                            (DeadLeafAST)
                        ])
                    ])
            assertEqual "sexprToAst" (expected) (result)),
        TestCase (do
            let originalFile = (File ["int b += a;"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = sexprToAst sexpr
            let expected = (AST [PlusEqualAST (AST [IntTypeAST, SymbolAST "b"]) (AST [SymbolAST "a"])])
            assertEqual "sexprToAst" (expected) (result)),
        TestCase (do
            let originalFile = (File ["int b -= a;"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = sexprToAst sexpr
            let expected = (AST [MinusEqualAST (AST [IntTypeAST, SymbolAST "b"]) (AST [SymbolAST "a"])])
            assertEqual "sexprToAst" (expected) (result)),
        TestCase (do
            let originalFile = (File ["int b *= a;"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = sexprToAst sexpr
            let expected = (AST [TimesEqualAST (AST [IntTypeAST, SymbolAST "b"]) (AST [SymbolAST "a"])])
            assertEqual "sexprToAst" (expected) (result)),
        TestCase (do
            let originalFile = (File ["int b /= a;"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = sexprToAst sexpr
            let expected = (AST [DivideEqualAST (AST [IntTypeAST, SymbolAST "b"]) (AST [SymbolAST "a"])])
            assertEqual "sexprToAst" (expected) (result)),
        TestCase (do
            let originalFile = (File ["int b %= a;"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = sexprToAst sexpr
            let expected = (AST [ModuloEqualAST (AST [IntTypeAST, SymbolAST "b"]) (AST [SymbolAST "a"])])
            assertEqual "sexprToAst" (expected) (result)),
        TestCase (do
            let originalFile = (File ["int a = 1; float b = 1.42; char c = 'c'; string d = \"Hello World!\""])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = sexprToAst sexpr
            let expected =
                    (AST [
                        AssignAST (AST [IntTypeAST, SymbolAST "a"]) (AST [IntAST 1]),
                        AssignAST (AST [FloatTypeAST, SymbolAST "b"]) (AST [FloatAST 1.42]),
                        AssignAST (AST [CharTypeAST, SymbolAST "c"]) (AST [CharAST 'c']),
                        AssignAST (AST [StringTypeAST, SymbolAST "d"]) (AST [StringAST "Hello World!"])
                    ])
            assertEqual "sexprToAst" (expected) (result)),
        TestCase (do
            let originalFile = (File ["#define my_int int"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = sexprToAst sexpr
            let expected = (AST [DefineAST "my_int" (AST [IntTypeAST])])
            assertEqual "sexprToAst" (expected) (result)),
        TestCase (do
            let originalFile = (File ["fun sum(int a, int b) : int { return (a + b); } int a = sum(1, 2);"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = sexprToAst sexpr
            let expected =
                    (AST [
                        FunAST "sum"
                            (AST [AST [IntTypeAST, SymbolAST "a"], AST [IntTypeAST, SymbolAST "b"]])
                            (FunTypeAST (AST [IntTypeAST]))
                            (AST [ReturnAST (PlusAST (AST [SymbolAST "a"]) (AST [SymbolAST "b"]))]),
                        AssignAST (AST [IntTypeAST, SymbolAST "a"]) (AST [SymbolAST "sum", (AST [AST [IntAST 1], AST [IntAST 2]])])
                    ])
            assertEqual "sexprToAst" (expected) (result)),
        TestCase (do
            let originalFile = (File ["while (i < 10) { i++; }"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = sexprToAst sexpr
            let expected = (AST [WhileAST (LessThanAST (AST [SymbolAST "i"]) (AST [IntAST 10])) (AST [IncrementAST (AST [SymbolAST "i"])])])
            assertEqual "sexprToAst" (expected) (result)),
        TestCase (do
            let originalFile = (File ["for (int i = 0; i < 10; i++) { print(i); }"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = sexprToAst sexpr
            let expected =
                    (AST [ForAST
                        (AssignAST (AST [IntTypeAST, SymbolAST "i"]) (AST [IntAST 0]))
                        (LessThanAST (AST [SymbolAST "i"]) (AST [IntAST 10]))
                        (IncrementAST (AST [SymbolAST "i"]))
                        (AST [AST [SymbolAST "print", (AST [SymbolAST "i"])]])
                    ])
            assertEqual "sexprToAst" (expected) (result)),
        TestCase (do
            let originalFile = (File ["int j = 1--; int i = j++;"])
            let cleanedFile = cleanFile originalFile False
            tokenList <- parseFile cleanedFile 0 [""] originalFile
            let sexpr = tokenListToSexpr $ tokenList
            let result = sexprToAst sexpr
            let expected = (AST [AssignAST (AST [IntTypeAST, SymbolAST "j"]) (DecrementAST (AST [IntAST 1])), AssignAST (AST [IntTypeAST, SymbolAST "i"]) (IncrementAST (AST [SymbolAST "j"]))])
            assertEqual "sexprToAst" (expected) (result))
    ]

testParsingFunction :: Test
testParsingFunction =
    TestList
        [
            TestLabel "showFile" showFileTest,
            TestLabel "showToken" showTokenTest,
            TestLabel "equalsToken" equalsTokenTest,
            TestLabel "equalsAST" equalsASTTest,
            TestLabel "printAST" printASTTest,

            TestLabel "parseKeyword" parseKeywordTest,
            TestLabel "parseIntToken" parseIntTokenTest,
            TestLabel "parseSymbolToken" parseSymbolTokenTest,
            TestLabel "parseStringToken" parseStringTokenTest,
            TestLabel "parseCharToken" parseCharTokenTest,
            TestLabel "parseToken" parseTokenTest,
            TestLabel "parseLine" parseLineTest,
            TestLabel "parseFile" parseFileTest,

            TestLabel "tokenListToSexpr" tokenListToSexprTest,

            TestLabel "splitAtValue" splitAtValueTest,
            TestLabel "splitAtLastValue" splitAtLastValueTest,

            TestLabel "getIfChain" getIfChainTest,

            TestLabel "binaryOperatorsAST" binaryOperatorsASTTest,
            TestLabel "operatorsAfterAST" operatorsAfterASTTest,
            TestLabel "operatorsBeforeAST" operatorsBeforeASTTest,
            TestLabel "listOperatorsASTCheck" listOperatorsASTCheckTest,

            TestLabel "pemdasTree" pemdasTreeTest,

            TestLabel "sexprToAst" sexprToAstTest,

            TestLabel "functorParser" functorParserTest,
            TestLabel "applicativeParser" applicativeParserTest,
            TestLabel "alternativeParser" alternativeParserTest,
            TestLabel "semigroupParser" semigroupParserTest,
            TestLabel "monoidParser" monoidParserTest,
            TestLabel "monadParser" monadParserTest,

            TestLabel "parseChar" parseCharTest,
            TestLabel "parseString" parseStringTest,
            TestLabel "parseAnyChar" parseAnyCharTest,
            TestLabel "parseOr" parseOrTest,
            TestLabel "parseAnd" parseAndTest,
            TestLabel "parseAndWith" parseAndWithTest,
            TestLabel "parseMany" parseManyTest,
            TestLabel "parseSome" parseSomeTest,
            TestLabel "parseUInt" parseUIntTest,
            TestLabel "parseInt" parseIntTest,
            TestLabel "parseEscapedChar" parseEscapedCharTest,
            TestLabel "parsePair" parsePairTest,
            TestLabel "parseOrBoth" parseOrBothTest,
            TestLabel "parseList" parseListTest
        ]

main :: IO ()
main = do
    putStrLn "Running tests..."
    putStrLn "Running Parsing tests..."
    _ <- runTestTT testParsingFunction
    putStrLn "Running other tests..."
    -- do other tests
    putStrLn "Done"
