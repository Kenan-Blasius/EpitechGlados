module UnitTestEvaluation (
    testEvaluationFunction
) where

import Test.HUnit
import BinaryEvaluation


testEvaluationFunction :: Test
testEvaluationFunction =
    TestList
        [
            TestLabel "evalFunction" testUnaryOpCall,
            TestLabel "compareOpCall" testCompareOpCall,
            TestLabel "toNewVariable" testToNewVariable,
            TestLabel "updateVariable" testUpdateVariable,
            TestLabel "lenOp" testLenOp,
            TestLabel "binaryOpCall" testBinaryOpCall,
            TestLabel "getLastIntFromStack" testGetLastIntFromStack,
            TestLabel "deleteLastValueFromStack" testDeleteLastValueFromStack,
            TestLabel "getLastAddressFromStack" testGetLastAddressFromStack,
            TestLabel "deleteUntilAddress" testDeleteUntilAddress,
            TestLabel "deleteUntilAddressExceptOne" testDeleteUntilAddressExceptOne,
            TestLabel "bytesToInt" testBytesToInt
        ]

testUnaryOpCall :: Test
testUnaryOpCall =
    TestList
    [
        TestCase (assertEqual "for unaryOpCall 45 [(IntType, MyInt 5)]"         [(IntType, MyInt (-5))]         (unaryOpCall 45 [(IntType, MyInt 5)])),
        TestCase (assertEqual "for unaryOpCall 45 [(FloatType, MyFloat 5.0)]"   [(FloatType, MyFloat (-5.0))]   (unaryOpCall 45 [(FloatType, MyFloat 5.0)])),
        TestCase (assertEqual "for unaryOpCall 45 [(CharType, MyChar 'a')]"     [(CharType, MyChar 'a')]        (unaryOpCall 45 [(CharType, MyChar 'a')])),
        TestCase (assertEqual "for unaryOpCall 33 [(IntType, MyInt 0)]"         [(IntType, MyInt 1)]            (unaryOpCall 33 [(IntType, MyInt 0)])),
        TestCase (assertEqual "for unaryOpCall 33 [(FloatType, MyFloat 0.0)]"   [(IntType, MyInt 1)]            (unaryOpCall 33 [(FloatType, MyFloat 0.0)])),
        TestCase (assertEqual "for unaryOpCall 33 [(CharType, MyChar '\0')]"    [(IntType, MyInt 1)]            (unaryOpCall 33 [(CharType, MyChar '\0')]))
    ]


-- * ----------------------------------------- CompareOpCall ----------------------------------------- * --

testCompareOpCall :: Test
testCompareOpCall =
    TestList
    [
        testCompareOpCallInt,
        testCompareOpCallFloat,
        testCompareOpCallChar,
        testCompareOpCallIntGreater,
        testCompareOpCallIntEqual,
        testCompareOpCallIntLessEqual,
        testCompareOpCallIntGreaterEqual,
        testCompareOpCallIntNotEqual
    ]


-- 60 <
-- 62 >
-- 61 ==
-- 97 <=
-- 98 >=
-- 33 !=
testCompareOpCallInt :: Test
testCompareOpCallInt = TestCase $ do
    assertEqual "for: compareOpCall 60 [(IntType, MyInt 2), (IntType, MyInt 1)]"
                [(IntType, MyInt 1)] -- 1 < 2 -> True
                (compareOpCall 60 [(IntType, MyInt 2), (IntType, MyInt 1)])

testCompareOpCallIntGreater :: Test
testCompareOpCallIntGreater = TestCase $ do
    assertEqual "for: compareOpCall 62 [(IntType, MyInt 2), (IntType, MyInt 1)]"
                [(IntType, MyInt 0)] -- 1 > 2 -> False
                (compareOpCall 62 [(IntType, MyInt 2), (IntType, MyInt 1)])

testCompareOpCallIntEqual :: Test
testCompareOpCallIntEqual = TestCase $ do
    assertEqual "for: compareOpCall 61 [(IntType, MyInt 2), (IntType, MyInt 2)]"
                [(IntType, MyInt 1)] -- 2 == 2 -> True
                (compareOpCall 61 [(IntType, MyInt 2), (IntType, MyInt 2)])

testCompareOpCallIntLessEqual :: Test
testCompareOpCallIntLessEqual = TestCase $ do
    assertEqual "for: compareOpCall 97 [(IntType, MyInt 2), (IntType, MyInt 2)]"
                [(IntType, MyInt 1)] -- 2 <= 2 -> True
                (compareOpCall 97 [(IntType, MyInt 2), (IntType, MyInt 2)])

testCompareOpCallIntGreaterEqual :: Test
testCompareOpCallIntGreaterEqual = TestCase $ do
    assertEqual "for: compareOpCall 98 [(IntType, MyInt 2), (IntType, MyInt 1)]"
                [(IntType, MyInt 1)] -- 2 >= 1 -> True
                (compareOpCall 98 [(IntType, MyInt 1), (IntType, MyInt 2)])

testCompareOpCallIntNotEqual :: Test
testCompareOpCallIntNotEqual = TestCase $ do
    assertEqual "for: compareOpCall 33 [(IntType, MyInt 2), (IntType, MyInt 1)]"
                [(IntType, MyInt 1)] -- 2 != 1 -> True
                (compareOpCall 33 [(IntType, MyInt 1), (IntType, MyInt 2)])

testCompareOpCallFloat :: Test
testCompareOpCallFloat = TestCase $ do
    assertEqual "for: compareOpCall 60 [(IntType, MyFloat 2.0), (IntType, MyFloat 1.0)]"
                [(IntType, MyInt 1)] -- 1.0 < 2.0 -> True
                (compareOpCall 60 [(IntType, MyFloat 2.0), (IntType, MyFloat 1.0)])

testCompareOpCallChar :: Test
testCompareOpCallChar = TestCase $ do
    assertEqual "for: compareOpCall 60 [(IntType, MyChar 'b'), (IntType, MyChar 'a')]"
                [(IntType, MyInt 1)] -- 'a' < 'b' -> True
                (compareOpCall 60 [(IntType, MyChar 'b'), (IntType, MyChar 'a')])


-- * ----------------------------------------- ToNewVariable ----------------------------------------- * --

testToNewVariable :: Test
testToNewVariable =
    TestList
    [
        testToNewVariableIntToInt,
        testToNewVariableStringToString,
        testToNewVariableCharToChar,
        testToNewVariableFloatToFloat,
        testToNewVariableIntToFloat,
        testToNewVariableIntToChar,
        testToNewVariableCharToInt
    ]

testToNewVariableIntToInt :: Test
testToNewVariableIntToInt = TestCase $ do
    assertEqual "for: toNewVariable 1 IntType IntType (MyInt 2)"
                (1, IntType, MyInt 2)
                (toNewVariable 1 IntType IntType (MyInt 2))

testToNewVariableStringToString :: Test
testToNewVariableStringToString = TestCase $ do
    assertEqual "for: toNewVariable 1 StringType StringType (MyString \"test\")"
                (1, StringType, MyString "test")
                (toNewVariable 1 StringType StringType (MyString "test"))

testToNewVariableCharToChar :: Test
testToNewVariableCharToChar = TestCase $ do
    assertEqual "for: toNewVariable 1 CharType CharType (MyChar 'a')"
                (1, CharType, MyChar 'a')
                (toNewVariable 1 CharType CharType (MyChar 'a'))

testToNewVariableFloatToFloat :: Test
testToNewVariableFloatToFloat = TestCase $ do
    assertEqual "for: toNewVariable 1 FloatType FloatType (MyFloat 2.0)"
                (1, FloatType, MyFloat 2.0)
                (toNewVariable 1 FloatType FloatType (MyFloat 2.0))

testToNewVariableIntToFloat :: Test
testToNewVariableIntToFloat = TestCase $ do
    assertEqual "for: toNewVariable 1 IntType FloatType (MyFloat 2.0)"
                (1, IntType, MyInt 2)
                (toNewVariable 1 IntType FloatType (MyFloat 2.0))

testToNewVariableIntToChar :: Test
testToNewVariableIntToChar = TestCase $ do
    assertEqual "for: toNewVariable 1 IntType CharType (MyChar 'a')"
                (1, IntType, MyInt 97)
                (toNewVariable 1 IntType CharType (MyChar 'a'))

testToNewVariableCharToInt :: Test
testToNewVariableCharToInt = TestCase $ do
    assertEqual "for: toNewVariable 1 CharType IntType (MyInt 97)"
                (1, CharType, MyChar 'a')
                (toNewVariable 1 CharType IntType (MyInt 97))


-- * ----------------------------------------- UpdateVariable ----------------------------------------- * --

testUpdateVariable :: Test
testUpdateVariable =
    TestList
    [
        testUpdateVariableEmpty,
        testUpdateVariableExisting,
        testUpdateVariableNew,
        testUpdateVariableString,
        testUpdateVariableChar,
        testUpdateVariableFloat
    ]

testUpdateVariableEmpty :: Test
testUpdateVariableEmpty = TestCase $ do
    assertEqual "for: updateVariable 1 IntType (MyInt 2) []"
                [(1, IntType, MyInt 2)]
                (updateVariable 1 IntType (MyInt 2) [])

testUpdateVariableExisting :: Test
testUpdateVariableExisting = TestCase $ do
    assertEqual "for: updateVariable 1 IntType (MyInt 2) [(1, IntType, MyInt 1), (2, IntType, MyInt 2)]"
                [(1, IntType, MyInt 2), (2, IntType, MyInt 2)]
                (updateVariable 1 IntType (MyInt 2) [(1, IntType, MyInt 1), (2, IntType, MyInt 2)])

testUpdateVariableNew :: Test
testUpdateVariableNew = TestCase $ do
    assertEqual "for: updateVariable 3 IntType (MyInt 3) [(1, IntType, MyInt 1), (2, IntType, MyInt 2)]"
                [(1, IntType, MyInt 1), (2, IntType, MyInt 2), (3, IntType, MyInt 3)]
                (updateVariable 3 IntType (MyInt 3) [(1, IntType, MyInt 1), (2, IntType, MyInt 2)])


testUpdateVariableString :: Test
testUpdateVariableString = TestCase $ do
    assertEqual "for: updateVariable 1 StringType (MyString \"test\") [(1, IntType, MyInt 1)"
                [(1, StringType, MyString "test")]
                (updateVariable 1 StringType (MyString "test") [(1, StringType, MyString "1")])

testUpdateVariableChar :: Test
testUpdateVariableChar = TestCase $ do
    assertEqual "for: updateVariable 2 CharType (MyChar 'a') [(1, IntType, MyInt 1), (2, IntType, MyInt 2)]"
                [(1, IntType, MyInt 1), (2, CharType, MyChar 'a')]
                (updateVariable 2 CharType (MyChar 'a') [(1, IntType, MyInt 1), (2, CharType, MyChar 'b')])

testUpdateVariableFloat :: Test
testUpdateVariableFloat = TestCase $ do
    assertEqual "for: updateVariable 3 FloatType (MyFloat 3.0) [(1, IntType, MyInt 1), (2, IntType, MyInt 2)]"
                [(1, IntType, MyInt 1), (2, IntType, MyInt 2), (3, FloatType, MyFloat 3.0)]
                (updateVariable 3 FloatType (MyFloat 3.0) [(1, IntType, MyInt 1), (2, IntType, MyInt 2)])


-- * ----------------------------------------- LenOp ----------------------------------------- * --

testLenOp :: Test
testLenOp = TestList [testLenOpLoadConst, testLenOpLoadVar, testLenOpStoreVar, testLenOpBinaryOp, testLenOpUnaryOp, testLenOpCompareOp, testLenOpJumpIfTrue, testLenOpJumpIfFalse, testLenOpJump, testLenOpJumpNewScope, testLenOpPop, testLenOpDup, testLenOpCall, testLenOpReturn, testLenOpLoadPc, testLenOpInvalid]

testLenOpLoadConst :: Test
testLenOpLoadConst = TestCase $ assertEqual "for: lenOp 0x01" 6 (lenOp 0x01)

testLenOpLoadVar :: Test
testLenOpLoadVar = TestCase $ assertEqual "for: lenOp 0x02" 6 (lenOp 0x02)

testLenOpStoreVar :: Test
testLenOpStoreVar = TestCase $ assertEqual "for: lenOp 0x03" 6 (lenOp 0x03)

testLenOpBinaryOp :: Test
testLenOpBinaryOp = TestCase $ assertEqual "for: lenOp 0x04" 2 (lenOp 0x04)

testLenOpUnaryOp :: Test
testLenOpUnaryOp = TestCase $ assertEqual "for: lenOp 0x05" 2 (lenOp 0x05)

testLenOpCompareOp :: Test
testLenOpCompareOp = TestCase $ assertEqual "for: lenOp 0x06" 2 (lenOp 0x06)

testLenOpJumpIfTrue :: Test
testLenOpJumpIfTrue = TestCase $ assertEqual "for: lenOp 0x07" 5 (lenOp 0x07)

testLenOpJumpIfFalse :: Test
testLenOpJumpIfFalse = TestCase $ assertEqual "for: lenOp 0x08" 5 (lenOp 0x08)

testLenOpJump :: Test
testLenOpJump = TestCase $ assertEqual "for: lenOp 0x09" 5 (lenOp 0x09)

testLenOpJumpNewScope :: Test
testLenOpJumpNewScope = TestCase $ assertEqual "for: lenOp 0x0A" 5 (lenOp 0x0A)

testLenOpPop :: Test
testLenOpPop = TestCase $ assertEqual "for: lenOp 0x0B" 1 (lenOp 0x0B)

testLenOpDup :: Test
testLenOpDup = TestCase $ assertEqual "for: lenOp 0x0C" 1 (lenOp 0x0C)

testLenOpCall :: Test
testLenOpCall = TestCase $ assertEqual "for: lenOp 0x0D" 2 (lenOp 0x0D)

testLenOpReturn :: Test
testLenOpReturn = TestCase $ assertEqual "for: lenOp 0x0E" 1 (lenOp 0x0E)

testLenOpLoadPc :: Test
testLenOpLoadPc = TestCase $ assertEqual "for: lenOp 0x0F" 1 (lenOp 0x0F)

testLenOpInvalid :: Test
testLenOpInvalid = TestCase $ assertEqual "for: lenOp 0x10" 0 (lenOp 0x10)


-- * ----------------------------------------- BinaryOpCall ----------------------------------------- * --


testBinaryOpCall :: Test
testBinaryOpCall =
    TestList
    [
        testBinaryOpCallPlusInt,
        testBinaryOpCallMinusFloat,
        testBinaryOpCallMultiplyInt,
        testBinaryOpCallDivideFloat,
        testBinaryOpCallConcatString,
        testBinaryOpCallBitwiseAndInt,
        testBinaryOpCallBitwiseOrChar,
        testBinaryOpCallDivideWithRemainder,
        testBinaryOpCallModulusChar,
        testBinaryOpCallConcatEmptyString,
        testBinaryOpCallSubtractNegativeResult,
        testBinaryOpCallDivideByZeroFloat,
        testBinaryOpCallBitwiseAndChar,
        testBinaryOpCallMultiplyFloat,
        testBinaryOpCallEmptyStack
    ]

testBinaryOpCallPLus :: Test
testBinaryOpCallPLus = TestCase $ do
    assertEqual "for: binaryOpCall 43 [(IntType, MyInt 1), (IntType, MyInt 2)]"
                [(IntType, MyInt 3)]
                (binaryOpCall 43 [(IntType, MyInt 1), (IntType, MyInt 2)])

testBinaryOpCallPlusInt :: Test
testBinaryOpCallPlusInt = TestCase $ do
    assertEqual "for: binaryOpCall 43 [(IntType, MyInt 1), (IntType, MyInt 2)]"
                [(IntType, MyInt 3)]
                (binaryOpCall 43 [(IntType, MyInt 1), (IntType, MyInt 2)])

testBinaryOpCallMinusFloat :: Test
testBinaryOpCallMinusFloat = TestCase $ do
    assertEqual "for: binaryOpCall 45 [(FloatType, MyFloat 2.0), (FloatType, MyFloat 1.0)]"
                [(FloatType, MyFloat (-1.0))]
                (binaryOpCall 45 [(FloatType, MyFloat 2.0), (FloatType, MyFloat 1.0)])

testBinaryOpCallMultiplyInt :: Test
testBinaryOpCallMultiplyInt = TestCase $ do
    assertEqual "for: binaryOpCall 42 [(IntType, MyInt 2), (IntType, MyInt 3)]"
                [(IntType, MyInt 6)]
                (binaryOpCall 42 [(IntType, MyInt 3), (IntType, MyInt 2)])

testBinaryOpCallDivideFloat :: Test
testBinaryOpCallDivideFloat = TestCase $ do
    assertEqual "for: binaryOpCall 47 [(FloatType, MyFloat 6.0), (FloatType, MyFloat 2.0)]"
                [(FloatType, MyFloat 3.0)]
                (binaryOpCall 47 [(FloatType, MyFloat 2.0), (FloatType, MyFloat 6.0)])

testBinaryOpCallConcatString :: Test
testBinaryOpCallConcatString = TestCase $ do
    assertEqual "for: binaryOpCall 43 [(StringType, MyString \"Hello \"), (StringType, MyString \"World!\")]"
                [(StringType, MyString "Hello World!")]
                (binaryOpCall 43 [(StringType, MyString "World!"), (StringType, MyString "Hello ")])

testBinaryOpCallBitwiseAndInt :: Test
testBinaryOpCallBitwiseAndInt = TestCase $ do
    assertEqual "for: binaryOpCall 38 [(IntType, MyInt 5), (IntType, MyInt 3)]"
                [(IntType, MyInt 1)]
                (binaryOpCall 38 [(IntType, MyInt 3), (IntType, MyInt 5)])

testBinaryOpCallBitwiseOrChar :: Test
testBinaryOpCallBitwiseOrChar = TestCase $ do
    assertEqual "for: binaryOpCall 124 [(CharType, MyChar 'a'), (CharType, MyChar 'b')]"
                [(CharType, MyChar 'c')]  -- The bitwise OR of 'a' and 'b' is 'c'
                (binaryOpCall 124 [(CharType, MyChar 'b'), (CharType, MyChar 'a')])

-- Test for division of integers with remainder
testBinaryOpCallDivideWithRemainder :: Test
testBinaryOpCallDivideWithRemainder = TestCase $ do
    assertEqual "for: binaryOpCall 47 [(IntType, MyInt 10), (IntType, MyInt 3)]"
                [(IntType, MyInt 3)] -- 10 / 3 = 3
                (binaryOpCall 47 [(IntType, MyInt 3), (IntType, MyInt 10)])

-- Test for modulus of characters
testBinaryOpCallModulusChar :: Test
testBinaryOpCallModulusChar = TestCase $ do
    assertEqual "for: binaryOpCall 37 [(CharType, MyChar 'z'), (CharType, MyChar 'a')]"
                [(CharType, MyChar 'a')]
                (binaryOpCall 37 [(CharType, MyChar 'z'), (CharType, MyChar 'a')])

-- Test for string concatenation with an empty string
testBinaryOpCallConcatEmptyString :: Test
testBinaryOpCallConcatEmptyString = TestCase $ do
    assertEqual "for: binaryOpCall 43 [(StringType, MyString \"Hello \"), (StringType, MyString \"\")]"
                [(StringType, MyString "Hello ")]
                (binaryOpCall 43 [(StringType, MyString "Hello "), (StringType, MyString "")])

-- Test for subtraction of integers with negative result
testBinaryOpCallSubtractNegativeResult :: Test
testBinaryOpCallSubtractNegativeResult = TestCase $ do
    assertEqual "for: binaryOpCall 45 [(IntType, MyInt 2), (IntType, MyInt 5)]"
                [(IntType, MyInt 3)] -- 5 - 2 = 3
                (binaryOpCall 45 [(IntType, MyInt 2), (IntType, MyInt 5)])

-- Test for division of floats with zero as divisor
testBinaryOpCallDivideByZeroFloat :: Test
testBinaryOpCallDivideByZeroFloat = TestCase $ do
    assertEqual "for: binaryOpCall 47 [(FloatType, MyFloat 3.0), (FloatType, MyFloat 0.0)]"
                [(FloatType, MyFloat 0.0)]  -- Infinity
                (binaryOpCall 47 [(FloatType, MyFloat 3.0), (FloatType, MyFloat 0.0)])

-- Test for bitwise AND of characters
testBinaryOpCallBitwiseAndChar :: Test
testBinaryOpCallBitwiseAndChar = TestCase $ do
    assertEqual "for: binaryOpCall 38 [(CharType, MyChar 'A'), (CharType, MyChar ' ')]"
                [(CharType, MyChar '\NUL')]  -- The bitwise AND of 'A' and ' ' is '\NUL' (null)
                (binaryOpCall 38 [(CharType, MyChar 'A'), (CharType, MyChar ' ')])

-- Test for multiplication of floats
testBinaryOpCallMultiplyFloat :: Test
testBinaryOpCallMultiplyFloat = TestCase $ do
    assertEqual "for: binaryOpCall 42 [(FloatType, MyFloat 2.5), (FloatType, MyFloat 3.0)]"
                [(FloatType, MyFloat 7.5)]
                (binaryOpCall 42 [(FloatType, MyFloat 2.5), (FloatType, MyFloat 3.0)])

-- Test for empty stack
testBinaryOpCallEmptyStack :: Test
testBinaryOpCallEmptyStack = TestCase $ do
    assertEqual "for: binaryOpCall 43 []"
                []
                (binaryOpCall 43 [])


-- * ----------------------------------------- getLastIntFromStack ----------------------------------------- * --

-- Test for getLastIntFromStack
testGetLastIntFromStack :: Test
testGetLastIntFromStack =
    TestList
    [
        testGetLastIntFromStackInt,
        testGetLastIntFromStackFloat,
        testGetLastIntFromStackChar
    ]

testGetLastIntFromStackInt :: Test
testGetLastIntFromStackInt =
    TestCase $ do
    let stack = [(IntType, MyInt 42), (FloatType, MyFloat 3.14), (CharType, MyChar 'A')]
    assertEqual "for: getLastIntFromStack" 42 (getLastIntFromStack stack)

testGetLastIntFromStackFloat :: Test
testGetLastIntFromStackFloat =
    TestCase $ do
    let stack = [(FloatType, MyFloat 3.14), (IntType, MyInt 42), (CharType, MyChar 'A')]
    assertEqual "for: getLastIntFromStack" 42 (getLastIntFromStack stack)

testGetLastIntFromStackChar :: Test
testGetLastIntFromStackChar =
    TestCase $ do
    let stack = [(CharType, MyChar 'A'), (IntType, MyInt 42), (FloatType, MyFloat 3.14)]
    assertEqual "for: getLastIntFromStack" 42 (getLastIntFromStack stack)



-- * ----------------------------------------- getLastFloatFromStack ----------------------------------------- * --

testDeleteLastValueFromStack :: Test
testDeleteLastValueFromStack =
    TestList
    [
        testDeleteLastValueFromStackInt,
        testDeleteLastValueFromStackFloat,
        testDeleteLastValueFromStackChar,
        testDeleteLastValueFromStackEmpty
    ]

testDeleteLastValueFromStackInt :: Test
testDeleteLastValueFromStackInt =
 TestCase $ do
    let stack = [(IntType, MyInt 42), (FloatType, MyFloat 3.14), (CharType, MyChar 'A')]
    assertEqual "for: deleteLastValueFromStack" [(FloatType, MyFloat 3.14), (CharType, MyChar 'A')] (deleteLastValueFromStack stack)

testDeleteLastValueFromStackFloat :: Test
testDeleteLastValueFromStackFloat =
 TestCase $ do
    let stack = [(FloatType, MyFloat 3.14), (IntType, MyInt 42), (CharType, MyChar 'A')]
    assertEqual "for: deleteLastValueFromStack" [(IntType, MyInt 42), (CharType, MyChar 'A')] (deleteLastValueFromStack stack)

testDeleteLastValueFromStackChar :: Test
testDeleteLastValueFromStackChar =
 TestCase $ do
    let stack = [(CharType, MyChar 'A'), (IntType, MyInt 42), (FloatType, MyFloat 3.14)]
    assertEqual "for: deleteLastValueFromStack" [(IntType, MyInt 42), (FloatType, MyFloat 3.14)] (deleteLastValueFromStack stack)

testDeleteLastValueFromStackEmpty :: Test
testDeleteLastValueFromStackEmpty =
 TestCase $ do
    let stack = []
    assertEqual "for: deleteLastValueFromStack" [] (deleteLastValueFromStack stack)


-- * ----------------------------------------- getLastFloatFromStack ----------------------------------------- * --


testGetLastAddressFromStack :: Test
testGetLastAddressFromStack =
    TestList
    [
        testGetLastAddressFromStackInt,
        testGetLastAddressFromStackFloat,
        testGetLastAddressFromStackChar
    ]

testGetLastAddressFromStackInt :: Test
testGetLastAddressFromStackInt = TestCase $ do
    let stack = [(AddressType, MyInt 100), (FloatType, MyFloat 3.14), (CharType, MyChar 'A')]
    assertEqual "for: getLastAddressFromStack" 100 (getLastAddressFromStack stack)

testGetLastAddressFromStackFloat :: Test
testGetLastAddressFromStackFloat = TestCase $ do
    let stack = [(FloatType, MyFloat 3.14), (AddressType, MyInt 100), (CharType, MyChar 'A')]
    assertEqual "for: getLastAddressFromStack" 100 (getLastAddressFromStack stack)

testGetLastAddressFromStackChar :: Test
testGetLastAddressFromStackChar = TestCase $ do
    let stack = [(CharType, MyChar 'A'), (FloatType, MyFloat 3.14), (AddressType, MyInt 100)]
    assertEqual "for: getLastAddressFromStack" 100 (getLastAddressFromStack stack)

-- * ----------------------------------------- deleteUntilAddress ----------------------------------------- * --

testDeleteUntilAddress :: Test
testDeleteUntilAddress =
    TestList
    [
        testDeleteUntilAddressEmpty,
        testDeleteUntilAddressInt,
        testDeleteUntilAddressChar
    ]


testDeleteUntilAddressEmpty :: Test
testDeleteUntilAddressEmpty = TestCase $ do
    let stack = [(IntType, MyInt 42), (AddressType, MyInt 100), (CharType, MyChar 'A')]
    assertEqual "for: deleteUntilAddress" [(CharType, MyChar 'A')] (deleteUntilAddress stack)

testDeleteUntilAddressInt :: Test
testDeleteUntilAddressInt = TestCase $ do
    let stack = [(IntType, MyInt 42), (AddressType, MyInt 100), (CharType, MyChar 'A')]
    assertEqual "for: deleteUntilAddress" [(CharType, MyChar 'A')] (deleteUntilAddress stack)

testDeleteUntilAddressChar :: Test
testDeleteUntilAddressChar = TestCase $ do
    let stack = [(CharType, MyChar 'A'), (AddressType, MyInt 100), (IntType, MyInt 42)]
    assertEqual "for: deleteUntilAddress" [(IntType, MyInt 42)] (deleteUntilAddress stack)



-- * ----------------------------------------- deleteUntilAddressExceptOne ----------------------------------------- * --

testDeleteUntilAddressExceptOne :: Test
testDeleteUntilAddressExceptOne =
    TestList
    [
        testDeleteUntilAddressExceptOneEmpty,
        testDeleteUntilAddressExceptOneInt,
        testDeleteUntilAddressExceptOneChar
    ]

--                                 address
--                                   |
-- deleteUntilAddressExceptOne stack 1

testDeleteUntilAddressExceptOneEmpty :: Test
testDeleteUntilAddressExceptOneEmpty = TestCase $ do
    let stack = [(IntType, MyInt 42), (AddressType, MyInt 100), (CharType, MyChar 'A'), (IntType, MyInt 42)]
    assertEqual "for: deleteUntilAddressExceptOne" [(CharType,MyChar 'A'),(IntType,MyInt 42)] (deleteUntilAddressExceptOne stack 1) -- del 0, 1 -> ['A', 42]

testDeleteUntilAddressExceptOneInt :: Test
testDeleteUntilAddressExceptOneInt = TestCase $ do
    let stack = [(IntType, MyInt 42), (CharType, MyChar 'A'), (StringType, MyString "test")]
    assertEqual "for: deleteUntilAddressExceptOne" [] (deleteUntilAddressExceptOne stack 2) -- del 0, 1, 2 -> []

testDeleteUntilAddressExceptOneChar :: Test
testDeleteUntilAddressExceptOneChar = TestCase $ do
    let stack = [(CharType, MyChar 'A'), (AddressType, MyInt 100), (IntType, MyInt 42)]
    assertEqual "for: deleteUntilAddressExceptOne" [(IntType, MyInt 42)] (deleteUntilAddressExceptOne stack 1) -- del 0, 1 -> [42]


-- * ----------------------------------------- bytesToInt ----------------------------------------- * --

testBytesToInt :: Test
testBytesToInt =
    TestList
    [
        testBytesToIntPositive,
        testBytesToIntNegative,
        testBytesToIntZero
    ]

testBytesToIntPositive :: Test
testBytesToIntPositive = TestCase $ do
    assertEqual "for: bytesToInt [0x01, 0x00, 0x00, 0x00]"
                1
                (bytesToInt [0x01, 0x00, 0x00, 0x00])

testBytesToIntNegative :: Test
testBytesToIntNegative = TestCase $ do
    assertEqual "for: bytesToInt [0xFF, 0xFF, 0xFF, 0xFF]"
                (-1)
                (bytesToInt [0xFF, 0xFF, 0xFF, 0xFF])

testBytesToIntZero :: Test
testBytesToIntZero = TestCase $ do
    assertEqual "for: bytesToInt [0x00, 0x00, 0x00, 0x00]"
                0
                (bytesToInt [0x00, 0x00, 0x00, 0x00])
