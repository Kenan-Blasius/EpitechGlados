module UnitTestCompiler (
    testCompilerFunction
) where

import Test.HUnit
import Types
import AstToBytecode

testCompilerFunction :: Test
testCompilerFunction =
    TestList
        [
            TestLabel "compile, return 0" testReturn,
            TestLabel "compile, variable declaration" testAssignAST,
            TestLabel "compile, if statement" testIfAST,
            TestLabel "compile, while statement" testWhileAST,
            TestLabel "compile, for statement" testForAST,
            TestLabel "compile, function declaration" testFunAST,
            TestLabel "compile, function call" testFunCallAST
        ]

testReturn :: Test
testReturn =
    TestList
        [
            let ast = AST []
            in let (_, bytecode, _) = astToBytecode' ast 0 []
            in TestCase (assertEqual "compile, return 0" (bytecode) ([]))
        ]

testAssignAST :: Test
testAssignAST =
    TestList
        [
            let ast = AST []
            in let (_, bytecode, _) = astToBytecode' ast 0 []
            in TestCase (assertEqual "compile, variable declaration" (bytecode) ([]))
        ]

testIfAST :: Test
testIfAST =
    TestList
        [
            let ast = AST []
            in let (_, bytecode, _) = astToBytecode' ast 0 []
            in TestCase (assertEqual "compile, if statement" (bytecode) ([]))
        ]

testWhileAST :: Test
testWhileAST =
    TestList
        [
            let ast = AST []
            in let (_, bytecode, _) = astToBytecode' ast 0 []
            in TestCase (assertEqual "compile, while statement" (bytecode) ([]))
        ]

testForAST :: Test
testForAST =
    TestList
        [
            let ast = AST []
            in let (_, bytecode, _) = astToBytecode' ast 0 []
            in TestCase (assertEqual "compile, for statement" (bytecode) ([]))
        ]

testFunAST :: Test
testFunAST =
    TestList
        [
            let ast = AST []
            in let (_, bytecode, _) = astToBytecode' ast 0 []
            in TestCase (assertEqual "compile, function declaration" (bytecode) ([]))
        ]

testFunCallAST :: Test
testFunCallAST =
    TestList
        [
            let ast = AST []
            in let (_, bytecode, _) = astToBytecode' ast 0 []
            in TestCase (assertEqual "compile, function call" (bytecode) ([]))
        ]

