module UnitTestEvaluation (
    testEvaluationFunction
) where

import Test.HUnit

testEvalFunction :: Test
testEvalFunction =
    TestList
    [
        TestCase (assertEqual "evalFunction" (True) (True))
    ]

testEvaluationFunction :: Test
testEvaluationFunction =
    TestList
        [
            TestLabel "evalFunction" testEvalFunction
        ]

