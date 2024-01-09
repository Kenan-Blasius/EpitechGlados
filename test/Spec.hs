import Test.HUnit
import UnitTestParser
import UnitTestCompiler
import UnitTestEvaluation

main :: IO ()
main = do
    putStrLn "Running tests..."
    putStrLn "Running Parsing tests..."
    _ <- runTestTT testParsingFunction
    putStrLn "Running Compiler tests..."
    _ <- runTestTT testCompilerFunction
    putStrLn "Running Eval tests..."
    _ <- runTestTT testEvaluationFunction
    putStrLn "Running other tests..."
    -- do other tests
    putStrLn "Done"
