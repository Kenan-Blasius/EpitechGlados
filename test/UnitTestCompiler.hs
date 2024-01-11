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
            let ast = AST [FunAST "main" (AST [IntTypeAST,SymbolAST "spain\127466\127480"]) (FunTypeAST (AST [IntTypeAST])) (ReturnAST (AST [IntAST 0]))]
            in let (_, bytecode, _) = astToBytecode' ast 0
            in TestCase (assertEqual "compile, return 0" (bytecode) ([FunEntryPoint "main" IntType,StoreVarBefore "spain🇪🇸" IntType,LoadConst 0 IntType,Return,Return]))
        ]

testAssignAST :: Test
testAssignAST =
    TestList
        [
            let ast = FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "france\127467\127479"]) (AST [IntAST 0]),PlusEqualAST (AST [SymbolAST "france\127467\127479"]) (AST [IntAST 5]),ReturnAST (AST [SymbolAST "france\127467\127479"])])
            in let (_, bytecode, _) = astToBytecode' ast 0
            in TestCase (assertEqual "compile, variable declaration" (bytecode) ([FunEntryPoint "main" IntType,LoadConst 0 IntType,StoreVarBefore "france🇫🇷" IntType,LoadVarBefore "france🇫🇷" UnknownType,LoadConst 5 IntType,BinaryOp "+",StoreVarBefore "france🇫🇷" UnknownType,LoadVarBefore "france🇫🇷" UnknownType,Return,Return]))
        ]

testIfAST :: Test
testIfAST =
    TestList
        [
            let ast = AST []
            in let (_, bytecode, _) = astToBytecode' ast 0
            in TestCase (assertEqual "compile, if statement" (bytecode) ([]))
        ]

testWhileAST :: Test
testWhileAST =
    TestList
        [
            let ast = FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "indian\127470\127475"]) (AST [IntAST 15]),WhileAST (GreaterThanAST (AST [SymbolAST "indian\127470\127475"]) (AST [IntAST 0])) (DecrementAST (AST [SymbolAST "indian\127470\127475"])),ReturnAST (AST [SymbolAST "indian\127470\127475"])])
            in let (_, bytecode, _) = astToBytecode' ast 0
            in TestCase (assertEqual "compile, while statement" (bytecode) ([FunEntryPoint "main" IntType,LoadConst 15 IntType,StoreVarBefore "indian🇮🇳" IntType,JumpRef 2,LoadVarBefore "indian🇮🇳" StringType,LoadConst 0 IntType,CompareOp ">",JumpIfFalseBefore 1,LoadVarBefore "indian🇮🇳" UnknownType,LoadConst 1 IntType,BinaryOp "-",StoreVarBefore "indian🇮🇳" UnknownType,JumpBefore 2,JumpRef 1,LoadVarBefore "indian🇮🇳" UnknownType,Return,Return]))
        ]

testForAST :: Test
testForAST =
    TestList
        [
            let ast = FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "mexico\127474\127485"]) (AST [IntAST 15]),ForAST (AssignAST (AST [IntTypeAST,SymbolAST "i"]) (AST [IntAST 0])) (LessThanAST (AST [SymbolAST "i"]) (AST [IntAST 5])) (IncrementAST (AST [SymbolAST "i"])) (PlusEqualAST (AST [SymbolAST "mexico\127474\127485"]) (AST [SymbolAST "i"])),ReturnAST (AST [SymbolAST "mexico\127474\127485"])])
            in let (_, bytecode, _) = astToBytecode' ast 0
            in TestCase (assertEqual "compile, for statement" (bytecode) ([]))
        ]

testFunAST :: Test
testFunAST =
    TestList
        [
            let ast = AST []
            in let (_, bytecode, _) = astToBytecode' ast 0
            in TestCase (assertEqual "compile, function declaration" (bytecode) ([FunEntryPoint "main" IntType,LoadConst 15 IntType,StoreVarBefore "mexico🇲🇽" IntType,LoadConst 0 IntType,StoreVarBefore "i" IntType,JumpRef 2,LoadVarBefore "i" StringType,LoadConst 5 IntType,CompareOp "<",JumpIfFalseBefore 1,LoadVarBefore "mexico🇲🇽" UnknownType,LoadVarBefore "i" UnknownType,BinaryOp "+",StoreVarBefore "mexico🇲🇽" UnknownType,LoadVarBefore "i" UnknownType,LoadConst 1 IntType,BinaryOp "+",StoreVarBefore "i" UnknownType,JumpBefore 2,JumpRef 1,LoadVarBefore "mexico🇲🇽" UnknownType,Return,Return]))
        ]

testFunCallAST :: Test
testFunCallAST =
    TestList
        [
            let ast = AST [FunAST "add" (AST [AST [IntTypeAST,SymbolAST "a"],AST [IntTypeAST,SymbolAST "b"]]) (FunTypeAST (AST [AST [IntTypeAST]])) (ReturnAST (PlusAST (AST [SymbolAST "a"]) (AST [SymbolAST "b"]))),FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "south_corea\127472\127479"]) (AST [IntAST 7]),AssignAST (AST [IntTypeAST,SymbolAST "north_corea\127472\127477"]) (AST [IntAST 3]),AssignAST (AST [IntTypeAST,SymbolAST "corea"]) (AST [SymbolAST "add",AST [AST [SymbolAST "south_corea\127472\127479"],AST [SymbolAST "north_corea\127472\127477"]]]),ReturnAST (AST [SymbolAST "corea"])])]
            in let (_, bytecode, _) = astToBytecode' ast 0
            in TestCase (assertEqual "compile, function call" (bytecode) ([FunEntryPoint "add" IntType,StoreVarBefore "a" IntType,StoreVarBefore "b" IntType,LoadVarBefore "a" UnknownType,LoadVarBefore "b" UnknownType,BinaryOp "+",Return,Return,FunEntryPoint "main" IntType,LoadConst 7 IntType,StoreVarBefore "south_corea🇰🇷" IntType,LoadConst 3 IntType,StoreVarBefore "north_corea🇰🇵" IntType,LoadVarBefore "south_corea🇰🇷" UnknownType,LoadVarBefore "north_corea🇰🇵" UnknownType,LoadPC,CallUserFun "add",StoreVarBefore "corea" IntType,LoadVarBefore "corea" UnknownType,Return,Return]))
        ]

