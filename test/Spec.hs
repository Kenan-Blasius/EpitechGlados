import Parser
import Test.HUnit

parseCharTest :: Test
parseCharTest =
    TestList
    [
        TestCase (assertEqual "parseChar" (Just ('a', "bcd"))           (runParser (parseChar 'a') "abcd")),
        TestCase (assertEqual "parseChar" Nothing                       (runParser (parseChar 'z') "bcd")),
        TestCase (assertEqual "parseChar" Nothing                       (runParser (parseChar 'b') "abcd")),
        TestCase (assertEqual "parseChar" (Just ('a', "aaa"))           (runParser (parseChar 'a') "aaaa"))
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

testParsingFunction :: Test
testParsingFunction =
    TestList
        [
            TestLabel "parseChar" parseCharTest,
            TestLabel "parseAnyChar" parseAnyCharTest,
            TestLabel "parseOr" parseOrTest,
            TestLabel "parseAnd" parseAndTest,
            TestLabel "parseAndWith" parseAndWithTest,
            TestLabel "parseMany" parseManyTest,
            TestLabel "parseSome" parseSomeTest,
            TestLabel "parseUInt" parseUIntTest,
            TestLabel "parseInt" parseIntTest,
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
