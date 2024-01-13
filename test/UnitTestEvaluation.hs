module UnitTestEvaluation (
    testEvaluationFunction
) where

import Test.HUnit
-- import TypesEvaluation
import BinaryEvaluation


testEvaluationFunction :: Test
testEvaluationFunction =
    TestList
        [
            TestLabel "evalFunction" testEvalFunction
        ]

testEvalFunction :: Test
testEvalFunction =
    TestList
    [
        TestCase (assertEqual "for unaryOpCall 45 [(IntType, MyInt 5)]"         [(IntType, MyInt (-5))]         (unaryOpCall 45 [(IntType, MyInt 5)])),
        -- Testing unary minus operation on floats
        TestCase (assertEqual "for unaryOpCall 45 [(FloatType, MyFloat 5.0)]"   [(FloatType, MyFloat (-5.0))]   (unaryOpCall 45 [(FloatType, MyFloat 5.0)])),
        -- Testing unary minus operation on chars
        TestCase (assertEqual "for unaryOpCall 45 [(CharType, MyChar 'a')]"     [(CharType, MyChar 'a')]        (unaryOpCall 45 [(CharType, MyChar 'a')])),
        -- Testing unary not operation on integers
        TestCase (assertEqual "for unaryOpCall 33 [(IntType, MyInt 0)]"         [(IntType, MyInt 1)]            (unaryOpCall 33 [(IntType, MyInt 0)])),
        -- Testing unary not operation on floats
        TestCase (assertEqual "for unaryOpCall 33 [(FloatType, MyFloat 0.0)]"   [(IntType, MyInt 1)]            (unaryOpCall 33 [(FloatType, MyFloat 0.0)])),
        -- Testing unary not operation on chars
        TestCase (assertEqual "for unaryOpCall 33 [(CharType, MyChar '\0')]"    [(IntType, MyInt 1)]            (unaryOpCall 33 [(CharType, MyChar '\0')]))
    ]
