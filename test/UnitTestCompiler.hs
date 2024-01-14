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
            TestLabel "compile, while statement" testTypesAST,
            TestLabel "compile, while statement" testWhileAST,
            TestLabel "compile, for statement" testForAST,
            TestLabel "compile, function declaration" testFunAST,
            TestLabel "compile, 9sumOfN" test9sumOfNAST,
            TestLabel "compile, 4while" test4whileAST,
            TestLabel "compile, 6function" test6functionAST,
            TestLabel "compile, 13fibonacci" test13fibonacciAST,
            TestLabel "compile, 2variables" test2variablesAST,
            TestLabel "compile, 2_5types" test2_5typesAST,
            TestLabel "compile, 7call" test7callAST,
            TestLabel "compile, 8recursivity" test8recursivityAST,
            TestLabel "compile, 10power" test10powerAST,
            TestLabel "compile, 11factorialWhileLoop" test11factorialWhileLoopAST,
            TestLabel "compile, 12gcd" test12gcdAST,
            TestLabel "compile, 14isPrime" test14isPrimeAST,
            TestLabel "compile, 15complex" test15complexAST
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

testTypesAST :: Test
testTypesAST =
    TestList
        [
            let ast = FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "indian\127470\127475"]) (AST [IntAST 15]),WhileAST (GreaterThanAST (AST [SymbolAST "indian\127470\127475"]) (AST [IntAST 0])) (DecrementAST (AST [SymbolAST "indian\127470\127475"])),ReturnAST (AST [SymbolAST "indian\127470\127475"])])
            in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
            in TestCase (assertEqual "compile, while statement" (bytecode) ([FunEntryPoint "main" IntType,LoadConst 15 IntType,StoreVarBefore "indian🇮🇳" IntType,JumpRef 2,LoadVarBefore "indian🇮🇳" UnknownType,LoadConst 0 IntType,CompareOp ">",JumpIfFalseBefore 1,LoadVarBefore "indian🇮🇳" UnknownType,LoadConst 1 IntType,BinaryOp "-",StoreVarBefore "indian🇮🇳" UnknownType,JumpBefore 2,JumpRef 1,LoadVarBefore "indian🇮🇳" UnknownType,Return,Return]))
        ]

testIfElseAST :: Test
testIfElseAST =
    TestList
        [
            let ast = FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "uk\127468\127463"]) (AST [IntAST 0]),IfAST (EqualAST (AST [SymbolAST "uk\127468\127463"]) (AST [IntAST 0])) (AssignAST (AST [SymbolAST "uk\127468\127463"]) (AST [IntAST 1])) (ElseIfAST (EqualAST (AST [SymbolAST "uk\127468\127463"]) (AST [IntAST 1])) (AssignAST (AST [SymbolAST "uk\127468\127463"]) (AST [IntAST 2])) (ElseAST (AssignAST (AST [SymbolAST "uk\127468\127463"]) (AST [IntAST 3])))),ReturnAST (AST [SymbolAST "uk\127468\127463"])])
            in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
            in TestCase (assertEqual "compile, for statement" (bytecode) ([FunEntryPoint "main" IntType,LoadConst 0 IntType,StoreVarBefore "uk🇬🇧" IntType,LoadVarBefore "uk🇬🇧" UnknownType,LoadConst 0 IntType,CompareOp "=",JumpIfFalseBefore 3,LoadConst 1 IntType,StoreVarBefore "uk🇬🇧" UnknownType,JumpBefore 4,JumpRef 3,LoadVarBefore "uk🇬🇧" UnknownType,LoadConst 1 IntType,CompareOp "=",JumpIfFalseBefore 1,LoadConst 2 IntType,StoreVarBefore "uk🇬🇧" UnknownType,JumpBefore 2,JumpRef 1,LoadConst 3 IntType,StoreVarBefore "uk🇬🇧" UnknownType,JumpRef 2,JumpRef 4,LoadVarBefore "uk🇬🇧" UnknownType,Return,Return]))
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
            let ast = FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "mexico\127474\127485"]) (AST [IntAST 15]),ForAST (AssignAST (AST [IntTypeAST,SymbolAST "i"]) (AST [IntAST 0])) (LessThanAST (AST [SymbolAST "i"]) (AST [IntAST 5])) (IncrementAST (AST [SymbolAST "i"])) (PlusEqualAST (AST [SymbolAST "mexico\127474\127485"]) (AST [SymbolAST "i"])),ReturnAST (AST [SymbolAST "mexico\127474\127485"])])
            in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
            in TestCase (assertEqual "compile, for statement" (bytecode) ([FunEntryPoint "main" IntType,LoadConst 15 IntType,StoreVarBefore "mexico🇲🇽" IntType,LoadConst 0 IntType,StoreVarBefore "i" IntType,JumpRef 2,LoadVarBefore "i" UnknownType,LoadConst 5 IntType,CompareOp "<",JumpIfFalseBefore 1,LoadVarBefore "mexico🇲🇽" UnknownType,LoadVarBefore "i" UnknownType,BinaryOp "+",StoreVarBefore "mexico🇲🇽" UnknownType,LoadVarBefore "i" UnknownType,LoadConst 1 IntType,BinaryOp "+",StoreVarBefore "i" UnknownType,JumpBefore 2,JumpRef 1,LoadVarBefore "mexico🇲🇽" UnknownType,Return,Return]))
        ]

testFunAST :: Test
testFunAST =
    TestList
        [
            let ast = AST [FunAST "add" (AST [AST [IntTypeAST,SymbolAST "a"],AST [IntTypeAST,SymbolAST "b"]]) (FunTypeAST (AST [AST [IntTypeAST]])) (ReturnAST (PlusAST (AST [SymbolAST "a"]) (AST [SymbolAST "b"]))),FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "south_corea\127472\127479"]) (AST [IntAST 7]),AssignAST (AST [IntTypeAST,SymbolAST "north_corea\127472\127477"]) (AST [IntAST 3]),AssignAST (AST [IntTypeAST,SymbolAST "corea"]) (AST [SymbolAST "add",AST [AST [SymbolAST "south_corea\127472\127479"],AST [SymbolAST "north_corea\127472\127477"]]]),ReturnAST (AST [SymbolAST "corea"])])]
            in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
            in TestCase (assertEqual "compile, function declaration" (bytecode) ([FunEntryPoint "add" IntType,StoreVarBefore "b" IntType,StoreVarBefore "a" IntType,LoadVarBefore "a" UnknownType,LoadVarBefore "b" UnknownType,BinaryOp "+",Return,Return,FunEntryPoint "main" IntType,LoadConst 7 IntType,StoreVarBefore "south_corea🇰🇷" IntType,LoadConst 3 IntType,StoreVarBefore "north_corea🇰🇵" IntType,LoadVarBefore "south_corea🇰🇷" UnknownType,LoadVarBefore "north_corea🇰🇵" UnknownType,LoadPC,CallUserFun "add",StoreVarBefore "corea" IntType,LoadVarBefore "corea" UnknownType,Return,Return]))
        ]


test9sumOfNAST :: Test
test9sumOfNAST =
	TestList
		[
			let ast = AST [FunAST "sumOfN" (AST [IntTypeAST,SymbolAST "n"]) (FunTypeAST (AST [AST [IntTypeAST]])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "sum"]) (AST [IntAST 0]),ForAST (AssignAST (AST [IntTypeAST,SymbolAST "i"]) (AST [IntAST 1])) (LessThanEqualAST (AST [SymbolAST "i"]) (AST [SymbolAST "n"])) (IncrementAST (AST [SymbolAST "i"])) (AssignAST (AST [SymbolAST "sum"]) (PlusAST (AST [SymbolAST "sum"]) (AST [SymbolAST "i"]))),ReturnAST (AST [SymbolAST "sum"])]),FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "colombia\127464\127476"]) (AST [IntAST 5]),AssignAST (AST [SymbolAST "colombia\127464\127476"]) (AST [SymbolAST "sumOfN",AST [SymbolAST "colombia\127464\127476"]]),ReturnAST (AST [SymbolAST "colombia\127464\127476"])])]
			in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
			in TestCase (assertEqual "compile, 9sumOfN" (bytecode) ([FunEntryPoint "sumOfN" IntType,StoreVarBefore "n" IntType,LoadConst 0 IntType,StoreVarBefore "sum" IntType,LoadConst 1 IntType,StoreVarBefore "i" IntType,JumpRef 2,LoadVarBefore "i" UnknownType,LoadVarBefore "n" UnknownType,CompareOp "a",JumpIfFalseBefore 1,LoadVarBefore "sum" UnknownType,LoadVarBefore "i" UnknownType,BinaryOp "+",StoreVarBefore "sum" UnknownType,LoadVarBefore "i" UnknownType,LoadConst 1 IntType,BinaryOp "+",StoreVarBefore "i" UnknownType,JumpBefore 2,JumpRef 1,LoadVarBefore "sum" UnknownType,Return,Return,FunEntryPoint "main" IntType,LoadConst 5 IntType,StoreVarBefore "colombia🇨🇴" IntType,LoadVarBefore "colombia🇨🇴" UnknownType,LoadPC,CallUserFun "sumOfN",StoreVarBefore "colombia🇨🇴" UnknownType,LoadVarBefore "colombia🇨🇴" UnknownType,Return,Return]))
		]
test4whileAST :: Test
test4whileAST =
	TestList
		[
			let ast = FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "indian\127470\127475"]) (AST [IntAST 15]),WhileAST (GreaterThanAST (AST [SymbolAST "indian\127470\127475"]) (AST [IntAST 0])) (DecrementAST (AST [SymbolAST "indian\127470\127475"])),ReturnAST (AST [SymbolAST "indian\127470\127475"])])
			in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
			in TestCase (assertEqual "compile, 4while" (bytecode) ([FunEntryPoint "main" IntType,LoadConst 15 IntType,StoreVarBefore "indian🇮🇳" IntType,JumpRef 2,LoadVarBefore "indian🇮🇳" UnknownType,LoadConst 0 IntType,CompareOp ">",JumpIfFalseBefore 1,LoadVarBefore "indian🇮🇳" UnknownType,LoadConst 1 IntType,BinaryOp "-",StoreVarBefore "indian🇮🇳" UnknownType,JumpBefore 2,JumpRef 1,LoadVarBefore "indian🇮🇳" UnknownType,Return,Return]))
		]
test6functionAST :: Test
test6functionAST =
	TestList
		[
			let ast = AST [FunAST "add" (AST [AST [IntTypeAST,SymbolAST "a"],AST [IntTypeAST,SymbolAST "b"]]) (FunTypeAST (AST [AST [IntTypeAST]])) (ReturnAST (PlusAST (AST [SymbolAST "a"]) (AST [SymbolAST "b"]))),FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "south_corea\127472\127479"]) (AST [IntAST 7]),AssignAST (AST [IntTypeAST,SymbolAST "north_corea\127472\127477"]) (AST [IntAST 3]),AssignAST (AST [IntTypeAST,SymbolAST "corea"]) (AST [SymbolAST "add",AST [AST [SymbolAST "south_corea\127472\127479"],AST [SymbolAST "north_corea\127472\127477"]]]),ReturnAST (AST [SymbolAST "corea"])])]
			in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
			in TestCase (assertEqual "compile, 6function" (bytecode) ([FunEntryPoint "add" IntType,StoreVarBefore "b" IntType,StoreVarBefore "a" IntType,LoadVarBefore "a" UnknownType,LoadVarBefore "b" UnknownType,BinaryOp "+",Return,Return,FunEntryPoint "main" IntType,LoadConst 7 IntType,StoreVarBefore "south_corea🇰🇷" IntType,LoadConst 3 IntType,StoreVarBefore "north_corea🇰🇵" IntType,LoadVarBefore "south_corea🇰🇷" UnknownType,LoadVarBefore "north_corea🇰🇵" UnknownType,LoadPC,CallUserFun "add",StoreVarBefore "corea" IntType,LoadVarBefore "corea" UnknownType,Return,Return]))
		]
test13fibonacciAST :: Test
test13fibonacciAST =
	TestList
		[
			let ast = AST [FunAST "fibonacci" (AST [IntTypeAST,SymbolAST "n"]) (FunTypeAST (AST [AST [IntTypeAST]])) (IfAST (OrAST (EqualAST (AST [SymbolAST "n"]) (AST [IntAST 0])) (EqualAST (AST [SymbolAST "n"]) (AST [IntAST 1]))) (ReturnAST (AST [SymbolAST "n"])) (ElseAST (ReturnAST (PlusAST (AST [SymbolAST "fibonacci",MinusAST (AST [SymbolAST "n"]) (AST [IntAST 1])]) (AST [SymbolAST "fibonacci",MinusAST (AST [SymbolAST "n"]) (AST [IntAST 2])]))))),FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "china\127464\127475"]) (AST [SymbolAST "fibonacci",AST [IntAST 10]]),ReturnAST (AST [SymbolAST "china\127464\127475"])])]
			in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
			in TestCase (assertEqual "compile, 13fibonacci" (bytecode) ([FunEntryPoint "fibonacci" IntType,StoreVarBefore "n" IntType,LoadVarBefore "n" UnknownType,LoadConst 0 IntType,CompareOp "=",LoadVarBefore "n" UnknownType,LoadConst 1 IntType,CompareOp "=",BinaryOp "|",JumpIfFalseBefore 1,LoadVarBefore "n" UnknownType,Return,JumpBefore 2,JumpRef 1,LoadVarBefore "n" UnknownType,LoadConst 1 IntType,BinaryOp "-",LoadPC,CallUserFun "fibonacci",LoadVarBefore "n" UnknownType,LoadConst 2 IntType,BinaryOp "-",LoadPC,CallUserFun "fibonacci",BinaryOp "+",Return,JumpRef 2,Return,FunEntryPoint "main" IntType,LoadConst 10 IntType,LoadPC,CallUserFun "fibonacci",StoreVarBefore "china🇨🇳" IntType,LoadVarBefore "china🇨🇳" UnknownType,Return,Return]))
		]
test2variablesAST :: Test
test2variablesAST =
	TestList
		[
			let ast = FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "france\127467\127479"]) (AST [IntAST 0]),PlusEqualAST (AST [SymbolAST "france\127467\127479"]) (AST [IntAST 5]),ReturnAST (AST [SymbolAST "france\127467\127479"])])
			in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
			in TestCase (assertEqual "compile, 2variables" (bytecode) ([FunEntryPoint "main" IntType,LoadConst 0 IntType,StoreVarBefore "france🇫🇷" IntType,LoadVarBefore "france🇫🇷" UnknownType,LoadConst 5 IntType,BinaryOp "+",StoreVarBefore "france🇫🇷" UnknownType,LoadVarBefore "france🇫🇷" UnknownType,Return,Return]))
		]
test2_5typesAST :: Test
test2_5typesAST =
	TestList
		[
			-- let ast = FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [FloatTypeAST,SymbolAST "brazil\127463\127479"]) (AST [FloatAST 7.2]),PlusEqualAST (AST [SymbolAST "brazil\127463\127479"]) (AST [FloatAST 2.8]),AssignAST (AST [IntTypeAST,SymbolAST "a"]) (AST [SymbolAST "brazil\127463\127479"]),ReturnAST (AST [SymbolAST "a"])])
			-- in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
			-- in TestCase (assertEqual "compile, 2_5types" (bytecode) ([FunEntryPoint "main" IntType,LoadConst 482125178470 FloatType,StoreVarBefore "brazil🇧🇷" FloatType,LoadVarBefore "brazil🇧🇷" UnknownType,LoadConst 1077097267 FloatType,BinaryOp "+",StoreVarBefore "brazil🇧🇷" UnknownType,LoadVarBefore "brazil🇧🇷" UnknownType,StoreVarBefore "a" IntType,LoadVarBefore "a" UnknownType,Return,Return]))
		]
test7callAST :: Test
test7callAST =
	TestList
		[
			let ast = FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "usa\127482\127480"]) (AST [IntAST 15]),AST [SymbolAST "print",AST [SymbolAST "usa\127482\127480"]],ReturnAST (AST [SymbolAST "usa\127482\127480"])])
			in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
			in TestCase (assertEqual "compile, 7call" (bytecode) ([FunEntryPoint "main" IntType,LoadConst 15 IntType,StoreVarBefore "usa🇺🇸" IntType,LoadVarBefore "usa🇺🇸" UnknownType,Call 1,LoadVarBefore "usa🇺🇸" UnknownType,Return,Return]))
		]
test8recursivityAST :: Test
test8recursivityAST =
	TestList
		[
			let ast = AST [FunAST "factorial" (AST [IntTypeAST,SymbolAST "n"]) (FunTypeAST (AST [AST [IntTypeAST]])) (IfAST (EqualAST (AST [SymbolAST "n"]) (AST [IntAST 0])) (ReturnAST (AST [IntAST 1])) (ElseAST (AST [AssignAST (AST [SymbolAST "n"]) (TimesAST (AST [SymbolAST "n"]) (AST [SymbolAST "factorial",MinusAST (AST [SymbolAST "n"]) (AST [IntAST 1])])),ReturnAST (AST [SymbolAST "n"])]))),FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "malaisia\127474\127486"]) (AST [IntAST 5]),AssignAST (AST [SymbolAST "malaisia\127474\127486"]) (AST [SymbolAST "factorial",AST [SymbolAST "malaisia\127474\127486"]]),ReturnAST (AST [SymbolAST "malaisia\127474\127486"])])]
			in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
			in TestCase (assertEqual "compile, 8recursivity" (bytecode) ([FunEntryPoint "factorial" IntType,StoreVarBefore "n" IntType,LoadVarBefore "n" UnknownType,LoadConst 0 IntType,CompareOp "=",JumpIfFalseBefore 1,LoadConst 1 IntType,Return,JumpBefore 2,JumpRef 1,LoadVarBefore "n" UnknownType,LoadVarBefore "n" UnknownType,LoadConst 1 IntType,BinaryOp "-",LoadPC,CallUserFun "factorial",BinaryOp "*",StoreVarBefore "n" UnknownType,LoadVarBefore "n" UnknownType,Return,JumpRef 2,Return,FunEntryPoint "main" IntType,LoadConst 5 IntType,StoreVarBefore "malaisia🇲🇾" IntType,LoadVarBefore "malaisia🇲🇾" UnknownType,LoadPC,CallUserFun "factorial",StoreVarBefore "malaisia🇲🇾" UnknownType,LoadVarBefore "malaisia🇲🇾" UnknownType,Return,Return]))
		]
test12gcdAST :: Test
test12gcdAST =
	TestList
		[
			let ast = AST [FunAST "gcd" (AST [AST [IntTypeAST,SymbolAST "a"],AST [IntTypeAST,SymbolAST "b"]]) (FunTypeAST (AST [AST [IntTypeAST]])) (AST [WhileAST (NotEqualAST (AST [SymbolAST "b"]) (AST [IntAST 0])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "temp"]) (AST [SymbolAST "b"]),AssignAST (AST [SymbolAST "b"]) (ModuloAST (AST [SymbolAST "a"]) (AST [SymbolAST "b"])),AssignAST (AST [SymbolAST "a"]) (AST [SymbolAST "temp"])]),ReturnAST (AST [SymbolAST "a"])]),FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "argentina\127462\127479"]) (AST [SymbolAST "gcd",AST [AST [IntAST 9],AST [IntAST 12]]]),ReturnAST (AST [SymbolAST "argentina\127462\127479"])])]
			in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
			in TestCase (assertEqual "compile, 12gcd" (bytecode) ([FunEntryPoint "gcd" IntType,StoreVarBefore "b" IntType,StoreVarBefore "a" IntType,JumpRef 2,LoadVarBefore "b" UnknownType,LoadConst 0 IntType,CompareOp "!",JumpIfFalseBefore 1,LoadVarBefore "b" UnknownType,StoreVarBefore "temp" IntType,LoadVarBefore "a" UnknownType,LoadVarBefore "b" UnknownType,BinaryOp "%",StoreVarBefore "b" UnknownType,LoadVarBefore "temp" UnknownType,StoreVarBefore "a" UnknownType,JumpBefore 2,JumpRef 1,LoadVarBefore "a" UnknownType,Return,Return,FunEntryPoint "main" IntType,LoadConst 9 IntType,LoadConst 12 IntType,LoadPC,CallUserFun "gcd",StoreVarBefore "argentina🇦🇷" IntType,LoadVarBefore "argentina🇦🇷" UnknownType,Return,Return]))
		]
test14isPrimeAST :: Test
test14isPrimeAST =
	TestList
		[
			let ast = AST [FunAST "isPrime" (AST [IntTypeAST,SymbolAST "n"]) (FunTypeAST (AST [AST [IntTypeAST]])) (AST [IfAST (LessThanEqualAST (AST [SymbolAST "n"]) (AST [IntAST 1])) (ReturnAST (AST [IntAST 0])) DeadLeafAST,ForAST (AssignAST (AST [IntTypeAST,SymbolAST "i"]) (AST [IntAST 2])) (DivideAST (LessThanEqualAST (AST [SymbolAST "i"]) (AST [SymbolAST "n"])) (AST [IntAST 2])) (IncrementAST (AST [SymbolAST "i"])) (IfAST (ModuloAST (AST [SymbolAST "n"]) (EqualAST (AST [SymbolAST "i"]) (AST [IntAST 0]))) (ReturnAST (AST [IntAST 0])) DeadLeafAST),ReturnAST (AST [IntAST 1])]),FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "japon\127471\127477"]) (AST [SymbolAST "isPrime",AST [IntAST 5]]),ReturnAST (AST [SymbolAST "japon\127471\127477"])])]
			in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
			in TestCase (assertEqual "compile, 14isPrime" (bytecode) ([FunEntryPoint "isPrime" IntType,StoreVarBefore "n" IntType,LoadVarBefore "n" UnknownType,LoadConst 1 IntType,CompareOp "a",JumpIfFalseBefore 1,LoadConst 0 IntType,Return,JumpRef 1,LoadConst 2 IntType,StoreVarBefore "i" IntType,JumpRef 4,LoadVarBefore "i" UnknownType,LoadVarBefore "n" UnknownType,CompareOp "a",LoadConst 2 IntType,BinaryOp "/",JumpIfFalseBefore 3,LoadVarBefore "n" UnknownType,LoadVarBefore "i" UnknownType,LoadConst 0 IntType,CompareOp "=",BinaryOp "%",JumpIfFalseBefore 2,LoadConst 0 IntType,Return,JumpRef 2,LoadVarBefore "i" UnknownType,LoadConst 1 IntType,BinaryOp "+",StoreVarBefore "i" UnknownType,JumpBefore 4,JumpRef 3,LoadConst 1 IntType,Return,Return,FunEntryPoint "main" IntType,LoadConst 5 IntType,LoadPC,CallUserFun "isPrime",StoreVarBefore "japon🇯🇵" IntType,LoadVarBefore "japon🇯🇵" UnknownType,Return,Return]))
		]
test10powerAST :: Test
test10powerAST =
	TestList
		[
			let ast = AST [FunAST "power" (AST [AST [IntTypeAST,SymbolAST "base"],AST [IntTypeAST,SymbolAST "exponent"]]) (FunTypeAST (AST [AST [IntTypeAST]])) (IfAST (EqualAST (AST [SymbolAST "exponent"]) (AST [IntAST 0])) (ReturnAST (AST [IntAST 1])) (ElseAST (ReturnAST (TimesAST (AST [SymbolAST "base"]) (AST [SymbolAST "power",AST [AST [SymbolAST "base"],MinusAST (AST [SymbolAST "exponent"]) (AST [IntAST 1])]]))))),FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "equador\127466\127464"]) (AST [SymbolAST "power",AST [AST [IntAST 2],AST [IntAST 4]]]),ReturnAST (AST [SymbolAST "equador\127466\127464"])])]
			in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
			in TestCase (assertEqual "compile, 10power" (bytecode) ([FunEntryPoint "power" IntType,StoreVarBefore "exponent" IntType,StoreVarBefore "base" IntType,LoadVarBefore "exponent" UnknownType,LoadConst 0 IntType,CompareOp "=",JumpIfFalseBefore 1,LoadConst 1 IntType,Return,JumpBefore 2,JumpRef 1,LoadVarBefore "base" UnknownType,LoadVarBefore "base" UnknownType,LoadVarBefore "exponent" UnknownType,LoadConst 1 IntType,BinaryOp "-",LoadPC,CallUserFun "power",BinaryOp "*",Return,JumpRef 2,Return,FunEntryPoint "main" IntType,LoadConst 2 IntType,LoadConst 4 IntType,LoadPC,CallUserFun "power",StoreVarBefore "equador🇪🇨" IntType,LoadVarBefore "equador🇪🇨" UnknownType,Return,Return]))
		]
test11factorialWhileLoopAST :: Test
test11factorialWhileLoopAST =
	TestList
		[
			let ast = AST [FunAST "factorialWhile" (AST [IntTypeAST,SymbolAST "n"]) (FunTypeAST (AST [AST [IntTypeAST]])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "result"]) (AST [IntAST 1]),WhileAST (GreaterThanAST (AST [SymbolAST "n"]) (AST [IntAST 0])) (AST [AssignAST (AST [SymbolAST "result"]) (TimesAST (AST [SymbolAST "result"]) (AST [SymbolAST "n"])),AssignAST (AST [SymbolAST "n"]) (MinusAST (AST [SymbolAST "n"]) (AST [IntAST 1]))]),ReturnAST (AST [SymbolAST "result"])]),FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "pakistan\127477\127472"]) (AST [SymbolAST "factorialWhile",AST [IntAST 5]]),ReturnAST (AST [SymbolAST "pakistan\127477\127472"])])]
			in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
			in TestCase (assertEqual "compile, 11factorialWhileLoop" (bytecode) ([FunEntryPoint "factorialWhile" IntType,StoreVarBefore "n" IntType,LoadConst 1 IntType,StoreVarBefore "result" IntType,JumpRef 2,LoadVarBefore "n" UnknownType,LoadConst 0 IntType,CompareOp ">",JumpIfFalseBefore 1,LoadVarBefore "result" UnknownType,LoadVarBefore "n" UnknownType,BinaryOp "*",StoreVarBefore "result" UnknownType,LoadVarBefore "n" UnknownType,LoadConst 1 IntType,BinaryOp "-",StoreVarBefore "n" UnknownType,JumpBefore 2,JumpRef 1,LoadVarBefore "result" UnknownType,Return,Return,FunEntryPoint "main" IntType,LoadConst 5 IntType,LoadPC,CallUserFun "factorialWhile",StoreVarBefore "pakistan🇵🇰" IntType,LoadVarBefore "pakistan🇵🇰" UnknownType,Return,Return]))
		]
test15complexAST :: Test
test15complexAST =
	TestList
		[
			let ast = AST [FunAST "add" (AST [IntTypeAST,SymbolAST "a"]) (FunTypeAST (AST [IntTypeAST])) (ReturnAST (PlusAST (AST [SymbolAST "a"]) (AST [IntAST 1]))),FunAST "sub" (AST [AST [IntTypeAST,SymbolAST "a"],AST [IntTypeAST,SymbolAST "b"]]) (FunTypeAST (AST [IntTypeAST])) (ReturnAST (MinusAST (AST [SymbolAST "a"]) (AST [SymbolAST "b"]))),FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "vatican\127483\127462"]) (EqualAST (AST [IntAST 3]) (AST [IntAST 2])),AssignAST (AST [IntTypeAST,SymbolAST "monaco\127474\127464"]) (AST [IntAST 4]),IfAST (EqualAST (AST [SymbolAST "add",AST [IntAST 1]]) (AST [SymbolAST "sub",AST [AST [SymbolAST "vatican\127483\127462"],AST [SymbolAST "monaco\127474\127464"]]])) (AssignAST (AST [SymbolAST "vatican\127483\127462"]) (AST [IntAST 42])) (ElseIfAST (EqualAST (AST [SymbolAST "add",AST [SymbolAST "vatican\127483\127462"]]) (AST [SymbolAST "add",AST [IntAST 0]])) (AST [AssignAST (AST [SymbolAST "vatican\127483\127462"]) (AST [SymbolAST "sub",AST [AST [SymbolAST "vatican\127483\127462"],AST [SymbolAST "monaco\127474\127464"]]]),WhileAST (LessThanAST (AST [SymbolAST "monaco\127474\127464"]) (AST [IntAST 10])) (AST [AssignAST (AST [SymbolAST "vatican\127483\127462"]) (AST [SymbolAST "add",AST [SymbolAST "vatican\127483\127462"]]),IncrementAST (AST [SymbolAST "monaco\127474\127464"])])]) (ElseAST (AssignAST (AST [SymbolAST "vatican\127483\127462"]) (AST [IntAST 1])))),ReturnAST (AST [SymbolAST "vatican\127483\127462"])])]
			in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
			in TestCase (assertEqual "compile, 15complex" (bytecode) ([FunEntryPoint "add" IntType,StoreVarBefore "a" IntType,LoadVarBefore "a" UnknownType,LoadConst 1 IntType,BinaryOp "+",Return,Return,FunEntryPoint "sub" IntType,StoreVarBefore "b" IntType,StoreVarBefore "a" IntType,LoadVarBefore "a" UnknownType,LoadVarBefore "b" UnknownType,BinaryOp "-",Return,Return,FunEntryPoint "main" IntType,LoadConst 3 IntType,LoadConst 2 IntType,CompareOp "=",StoreVarBefore "vatican🇻🇦" IntType,LoadConst 4 IntType,StoreVarBefore "monaco🇲🇨" IntType,LoadConst 1 IntType,LoadPC,CallUserFun "add",LoadVarBefore "vatican🇻🇦" UnknownType,LoadVarBefore "monaco🇲🇨" UnknownType,LoadPC,CallUserFun "sub",CompareOp "=",JumpIfFalseBefore 5,LoadConst 42 IntType,StoreVarBefore "vatican🇻🇦" UnknownType,JumpBefore 6,JumpRef 5,LoadVarBefore "vatican🇻🇦" UnknownType,LoadPC,CallUserFun "add",LoadConst 0 IntType,LoadPC,CallUserFun "add",CompareOp "=",JumpIfFalseBefore 3,LoadVarBefore "vatican🇻🇦" UnknownType,LoadVarBefore "monaco🇲🇨" UnknownType,LoadPC,CallUserFun "sub",StoreVarBefore "vatican🇻🇦" UnknownType,JumpRef 2,LoadVarBefore "monaco🇲🇨" UnknownType,LoadConst 10 IntType,CompareOp "<",JumpIfFalseBefore 1,LoadVarBefore "vatican🇻🇦" UnknownType,LoadPC,CallUserFun "add",StoreVarBefore "vatican🇻🇦" UnknownType,LoadVarBefore "monaco🇲🇨" UnknownType,LoadConst 1 IntType,BinaryOp "+",StoreVarBefore "monaco🇲🇨" UnknownType,JumpBefore 2,JumpRef 1,JumpBefore 4,JumpRef 3,LoadConst 1 IntType,StoreVarBefore "vatican🇻🇦" UnknownType,JumpRef 4,JumpRef 6,LoadVarBefore "vatican🇻🇦" UnknownType,Return,Return]))
		]
test3if_elseAST :: Test
test3if_elseAST =
	TestList
		[
			let ast = FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "uk\127468\127463"]) (AST [IntAST 0]),IfAST (EqualAST (AST [SymbolAST "uk\127468\127463"]) (AST [IntAST 0])) (AssignAST (AST [SymbolAST "uk\127468\127463"]) (AST [IntAST 1])) (ElseIfAST (EqualAST (AST [SymbolAST "uk\127468\127463"]) (AST [IntAST 1])) (AssignAST (AST [SymbolAST "uk\127468\127463"]) (AST [IntAST 2])) (ElseAST (AssignAST (AST [SymbolAST "uk\127468\127463"]) (AST [IntAST 3])))),ReturnAST (AST [SymbolAST "uk\127468\127463"])])
			in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
			in TestCase (assertEqual "compile, 3if_else" (bytecode) ([FunEntryPoint "main" IntType,LoadConst 0 IntType,StoreVarBefore "uk🇬🇧" IntType,LoadVarBefore "uk🇬🇧" UnknownType,LoadConst 0 IntType,CompareOp "=",JumpIfFalseBefore 3,LoadConst 1 IntType,StoreVarBefore "uk🇬🇧" UnknownType,JumpBefore 4,JumpRef 3,LoadVarBefore "uk🇬🇧" UnknownType,LoadConst 1 IntType,CompareOp "=",JumpIfFalseBefore 1,LoadConst 2 IntType,StoreVarBefore "uk🇬🇧" UnknownType,JumpBefore 2,JumpRef 1,LoadConst 3 IntType,StoreVarBefore "uk🇬🇧" UnknownType,JumpRef 2,JumpRef 4,LoadVarBefore "uk🇬🇧" UnknownType,Return,Return]))
		]
test5forAST :: Test
test5forAST =
	TestList
		[
			let ast = FunAST "main" DeadLeafAST (FunTypeAST (AST [IntTypeAST])) (AST [AssignAST (AST [IntTypeAST,SymbolAST "mexico\127474\127485"]) (AST [IntAST 15]),ForAST (AssignAST (AST [IntTypeAST,SymbolAST "i"]) (AST [IntAST 0])) (LessThanAST (AST [SymbolAST "i"]) (AST [IntAST 5])) (IncrementAST (AST [SymbolAST "i"])) (PlusEqualAST (AST [SymbolAST "mexico\127474\127485"]) (AST [SymbolAST "i"])),ReturnAST (AST [SymbolAST "mexico\127474\127485"])])
			in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
			in TestCase (assertEqual "compile, 5for" (bytecode) ([FunEntryPoint "main" IntType,LoadConst 15 IntType,StoreVarBefore "mexico🇲🇽" IntType,LoadConst 0 IntType,StoreVarBefore "i" IntType,JumpRef 2,LoadVarBefore "i" UnknownType,LoadConst 5 IntType,CompareOp "<",JumpIfFalseBefore 1,LoadVarBefore "mexico🇲🇽" UnknownType,LoadVarBefore "i" UnknownType,BinaryOp "+",StoreVarBefore "mexico🇲🇽" UnknownType,LoadVarBefore "i" UnknownType,LoadConst 1 IntType,BinaryOp "+",StoreVarBefore "i" UnknownType,JumpBefore 2,JumpRef 1,LoadVarBefore "mexico🇲🇽" UnknownType,Return,Return]))
		]
test1returnAST :: Test
test1returnAST =
	TestList
		[
			let ast = FunAST "main" (AST [IntTypeAST,SymbolAST "spain\127466\127480"]) (FunTypeAST (AST [IntTypeAST])) (ReturnAST (AST [IntAST 0]))
			in let (_, bytecode, _) = astToBytecode' ast 0 (getListOfFunctions ast)
			in TestCase (assertEqual "compile, 1return" (bytecode) ([FunEntryPoint "main" IntType,StoreVarBefore "spain🇪🇸" IntType,LoadConst 0 IntType,Return,Return]))
		]

