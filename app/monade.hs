
import Control.Monad.State

type Counter = Int

-- Define the operations
data Operation = Increment | Decrement | Reset deriving (Show)

-- Define a function that performs an operation on the counter
performOperation :: Operation -> State Counter ()
performOperation operation = do
    counter <- get
    case operation of
        Increment -> put (counter + 1)
        Decrement -> put (counter - 1)
        Reset     -> put 0

-- Define a function that performs a list of operations
performOperations :: [Operation] -> State Counter ()
performOperations []     = return ()
performOperations (o:os) = performOperation o >> performOperations os

main :: IO ()
main = do
    let operations = [Increment, Increment, Increment]
    let finalState = execState (performOperations operations) 0
    putStrLn $ "The final state of the counter is: " ++ show finalState