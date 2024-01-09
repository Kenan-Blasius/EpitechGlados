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
            let ast = AST [FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [ReturnAST (AST [IntAST 0])])]
            in let (_, bytecode, _) = astToBytecode' ast 0
            in TestCase (assertEqual "compile, return 0" (bytecode) ([Jump 37,LoadConst 0,Return,Return]))
        ]

testAssignAST :: Test
testAssignAST =
    TestList
        [
            let ast = AST [FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AST [IntTypeAST,SymbolAST "a"],AssignAST (AST [IntTypeAST,SymbolAST "b"]) (AST [IntAST 2]),AssignAST (AST [SymbolAST "a"]) (AST [IntAST 1])])]
            in let (_, bytecode, _) = astToBytecode' ast 0
            in TestCase (assertEqual "compile, variable declaration" (bytecode) ([Jump 37,LoadVar "a",LoadConst 2,StoreVar "b",LoadConst 1,StoreVar "a",Return]))
        ]

testIfAST :: Test
testIfAST =
    TestList
        [
            let ast = AST [FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "a"]) (AST [IntAST 1]),IfAST (EqualAST (AST [SymbolAST "a"]) (AST [IntAST 1])) (AST [ReturnAST (AST [IntAST 1])]) (AST [ElseAST (AST [ReturnAST (AST [IntAST 0])])])])]
            in let (_, bytecode, _) = astToBytecode' ast 0
            in TestCase (assertEqual "compile, if statement" (bytecode) ([Jump 37,LoadConst 1,StoreVar "a",LoadVar "a",LoadConst 1,CompareOp "==",JumpIfFalse 69,LoadConst 1,Return,Jump 75,LoadConst 0,Return,Return]))
        ]

testWhileAST :: Test
testWhileAST =
    TestList
        [
            let ast = AST [FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "a"]) (AST [IntAST 1]),WhileAST (LessThanAST (AST [SymbolAST "a"]) (AST [IntAST 10])) (AST [AssignAST (AST [SymbolAST "a"]) (PlusAST (AST [SymbolAST "a"]) (AST [IntAST 1]))])])]
            in let (_, bytecode, _) = astToBytecode' ast 0
            in TestCase (assertEqual "compile, while statement" (bytecode) ([Jump 37,LoadConst 1,StoreVar "a",LoadVar "a",LoadConst 10,CompareOp "<",JumpIfFalse 74,LoadVar "a",LoadConst 1,BinaryOp "+",StoreVar "a",Jump 44,Return]))
        ]

testForAST :: Test
testForAST =
    TestList
        [
            let ast = AST [FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "a"]) (AST [IntAST 1]),ForAST (AssignAST (AST [SymbolAST "a"]) (AST [IntAST 1])) (LessThanAST (AST [SymbolAST "a"]) (AST [IntAST 10])) (AssignAST (AST [SymbolAST "a"]) (PlusAST (AST [SymbolAST "a"]) (AST [IntAST 1]))) (AST [AssignAST (AST [SymbolAST "a"]) (PlusAST (AST [SymbolAST "a"]) (AST [IntAST 1]))])])]
            in let (_, bytecode, _) = astToBytecode' ast 0
            in TestCase (assertEqual "compile, for statement" (bytecode) ([Jump 37,LoadConst 1,StoreVar "a",LoadConst 1,StoreVar "a",LoadVar "a",LoadConst 10,CompareOp "<",JumpIfFalse 92,LoadVar "a",LoadConst 1,BinaryOp "+",StoreVar "a",LoadVar "a",LoadConst 1,BinaryOp "+",StoreVar "a",Jump 51,Return]))
        ]

testFunAST :: Test
testFunAST =
    TestList
        [
            let ast = AST [FunAST "funky" (AST [IntTypeAST,SymbolAST "b"]) (FunTypeAST (AST [IntTypeAST])) (AST [ReturnAST (AST [SymbolAST "b"])]),FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "a"]) (AST [IntAST 1]),AssignAST (AST [SymbolAST "a"]) (AST [SymbolAST "funky",AST [SymbolAST "a"]]),ReturnAST (AST [SymbolAST "a"])])]
            in let (_, bytecode, _) = astToBytecode' ast 0
            in TestCase (assertEqual "compile, function declaration" (bytecode) ([Jump 43,StoreVar "b",LoadVar "b",Return,Return,LoadConst 1,StoreVar "a",LoadVar "a",LoadPC,JumpNewScope 37 ,StoreVar "a",LoadVar "a",Return,Return]))
        ]

testFunCallAST :: Test
testFunCallAST =
    TestList
        [
            let ast = AST [FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AST [SymbolAST "exit",AST [IntAST 0]]])]
            in let (_, bytecode, _) = astToBytecode' ast 0
            in TestCase (assertEqual "compile, function call" (bytecode) ([Jump 37,LoadConst 0,Call 60,Return]))
        ]

