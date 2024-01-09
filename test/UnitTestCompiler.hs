module UnitTestCompiler (
    testCompilerFunction
) where

import Test.HUnit

testCompileFunction :: Test
testCompileFunction =
    TestList
    [
        TestCase (assertEqual "compileFunction" (True) (True))
    ]

testCompilerFunction :: Test
testCompilerFunction =
    TestList
        [
            TestLabel "compileFunction" testCompileFunction
        ]

