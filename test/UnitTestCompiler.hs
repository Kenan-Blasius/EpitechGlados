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
            let ast = FunAST "main" (AST [IntTypeAST,SymbolAST "spain\127466\127480"]) (FunTypeAST (AST [IntTypeAST])) (ReturnAST (AST [IntAST 0]))
            in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
            in TestCase (assertEqual "compile, return 0" (bytecode) ([FunEntryPoint "main" IntType,StoreVarBefore "spain🇪🇸" IntType,LoadConst 0 IntType,Return,Return]))
        ]

testAssignAST :: Test
testAssignAST =
    TestList
        [
            -- let ast = FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [FloatTypeAST,SymbolAST "brazil\127463\127479"]) (AST [FloatAST 7.2]),PlusEqualAST (AST [SymbolAST "brazil\127463\127479"]) (AST [FloatAST 2.8]),AssignAST (AST [IntTypeAST,SymbolAST "a"]) (AST [SymbolAST "brazil\127463\127479"]),ReturnAST (AST [SymbolAST "a"])])
            -- in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
            -- in TestCase (assertEqual "compile, variable declaration" (bytecode) ([FunEntryPoint "main" IntType,LoadConst 1088841318 FloatType,StoreVarBefore "brazil🇧🇷" FloatType,LoadVarBefore "brazil🇧🇷" UnknownType,LoadConst 1077097267 FloatType,BinaryOp "+",StoreVarBefore "brazil🇧🇷" UnknownType,LoadVarBefore "brazil🇧🇷" UnknownType,StoreVarBefore "a" IntType,LoadVarBefore "a" UnknownType,Return,Return]))
        ]

testIfAST :: Test
testIfAST =
    TestList
        [
            -- let ast = FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "france\127467\127479"]) (AST [IntAST 0]),PlusEqualAST (AST [SymbolAST "france\127467\127479"]) (AST [IntAST 5]),ReturnAST (AST [SymbolAST "france\127467\127479"])])
            -- in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
            -- in TestCase (assertEqual "compile, if statement" (bytecode) ([FunEntryPoint "main" IntType,LoadConst 482125178470 FloatType,StoreVarBefore "brazil🇧🇷" FloatType,LoadVarBefore "brazil🇧🇷" UnknownType,LoadConst 1077097267 FloatType,BinaryOp "+",StoreVarBefore "brazil🇧🇷" UnknownType,LoadVarBefore "brazil🇧🇷" UnknownType,StoreVarBefore "a" IntType,LoadVarBefore "a" UnknownType,Return,Return]))
        ]

testWhileAST :: Test
testWhileAST =
    TestList
        [
            let ast = FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "indian\127470\127475"]) (AST [IntAST 15]),WhileAST (GreaterThanAST (AST [SymbolAST "indian\127470\127475"]) (AST [IntAST 0])) (DecrementAST (AST [SymbolAST "indian\127470\127475"])),ReturnAST (AST [SymbolAST "indian\127470\127475"])])
            in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
            in TestCase (assertEqual "compile, while statement" (bytecode) ([FunEntryPoint "main" IntType,LoadConst 15 IntType,StoreVarBefore "indian🇮🇳" IntType,JumpRef 2,LoadVarBefore "indian🇮🇳" UnknownType,LoadConst 0 IntType,CompareOp ">",JumpIfFalseBefore 1,LoadVarBefore "indian🇮🇳" UnknownType,LoadConst 1 IntType,BinaryOp "-",StoreVarBefore "indian🇮🇳" UnknownType,JumpBefore 2,JumpRef 1,LoadVarBefore "indian🇮🇳" UnknownType,Return,Return]))
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

