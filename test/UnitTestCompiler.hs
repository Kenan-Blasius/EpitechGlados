module UnitTestCompiler (
    testCompilerFunction
) where

import Test.HUnit
import Types
import BytecodeToBinary
import AstToBytecode

testCompilerFunction :: Test
testCompilerFunction =
    TestList
        [
            TestLabel "compile, return 0" testCompiler
        ]

testCompiler :: Test
testCompiler =
    TestList
        [
            let ast = AST [FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [ReturnAST (AST [IntAST 0])])]
            in let (_, bytecode, _) = astToBytecode' ast 0
            in TestCase (assertEqual "compile, return 0" (bytecode) ([Jump 37,LoadConst 0,Return,Return]))
        ]