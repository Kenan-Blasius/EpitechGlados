module Eval (evalAST) where

import Types
import Debug.Trace


-- * -------------------------------- COMPARE -------------------------------- * --

compareEqualAST :: Environment -> AST -> AST -> (Environment, AST)
compareEqualAST env x y = 
    let (env1, evaluatedX) = evalAST env x
        (env2, evaluatedY) = evalAST env1 y
    in case (evaluatedX, evaluatedY) of
        (IntAST x', IntAST y') -> (env2, if x' == y' then IntAST 1 else IntAST 0)
        (SymbolAST x', SymbolAST y') -> (env2, if x' == y' then IntAST 1 else IntAST 0)
        (SymbolAST x', IntAST y') -> (env2, if x' == show y' then IntAST 1 else IntAST 0)
        (IntAST x', SymbolAST y') -> (env2, if show x' == y' then IntAST 1 else IntAST 0)
        _ -> (env2, DeadLeafAST)


compareGreaterAST :: Environment -> AST -> AST -> (Environment, AST)
compareGreaterAST env x y = evalGreaterAST env x y

evalGreaterAST :: Environment -> AST -> AST -> (Environment, AST)
evalGreaterAST env x y =
    let (env1, evaluatedX) = evalAST env x
        (env2, evaluatedY) = evalAST env1 y
    in case (evaluatedX, evaluatedY) of
        (IntAST x', IntAST y') -> (env2, if x' > y' then IntAST 1 else IntAST 0)
        (SymbolAST x', SymbolAST y') -> (env2, if x' > y' then IntAST 1 else IntAST 0)
        (SymbolAST x', IntAST y') -> (env2, if x' > show y' then IntAST 1 else IntAST 0)
        (IntAST x', SymbolAST y') -> (env2, if show x' > y' then IntAST 1 else IntAST 0)
        _ -> (env2, DeadLeafAST)


compareLessAST :: Environment -> AST -> AST -> (Environment, AST)
compareLessAST env x y = evalLessAST env x y

evalLessAST :: Environment -> AST -> AST -> (Environment, AST)
evalLessAST env x y =
    let (env1, evaluatedX) = evalAST env x
        (env2, evaluatedY) = evalAST env1 y
    in case (evaluatedX, evaluatedY) of
        (IntAST x', IntAST y') -> (env2, if x' < y' then IntAST 1 else IntAST 0)
        (SymbolAST x', SymbolAST y') -> (env2, if x' < y' then IntAST 1 else IntAST 0)
        (SymbolAST x', IntAST y') -> (env2, if x' < show y' then IntAST 1 else IntAST 0)
        (IntAST x', SymbolAST y') -> (env2, if show x' < y' then IntAST 1 else IntAST 0)
        _ -> (env2, DeadLeafAST)


compareLessEqualAST :: Environment -> AST -> AST -> (Environment, AST)
compareLessEqualAST env x y = evalLessEqualAST env x y

evalLessEqualAST :: Environment -> AST -> AST -> (Environment, AST)
evalLessEqualAST env x y =
    let (env1, evaluatedX) = evalAST env x
        (env2, evaluatedY) = evalAST env1 y
    in case (evaluatedX, evaluatedY) of
        (IntAST x', IntAST y') -> (env2, if x' <= y' then IntAST 1 else IntAST 0)
        (SymbolAST x', SymbolAST y') -> (env2, if x' <= y' then IntAST 1 else IntAST 0)
        (SymbolAST x', IntAST y') -> (env2, if x' <= show y' then IntAST 1 else IntAST 0)
        (IntAST x', SymbolAST y') -> (env2, if show x' <= y' then IntAST 1 else IntAST 0)
        _ -> (env2, DeadLeafAST)


compareGreaterEqualAST :: Environment -> AST -> AST -> (Environment, AST)
compareGreaterEqualAST env x y = evalGreaterEqualAST env x y

evalGreaterEqualAST :: Environment -> AST -> AST -> (Environment, AST)
evalGreaterEqualAST env x y =
    let (env1, evaluatedX) = evalAST env x
        (env2, evaluatedY) = evalAST env1 y
    in case (evaluatedX, evaluatedY) of
        (IntAST x', IntAST y') -> (env2, if x' >= y' then IntAST 1 else IntAST 0)
        (SymbolAST x', SymbolAST y') -> (env2, if x' >= y' then IntAST 1 else IntAST 0)
        (SymbolAST x', IntAST y') -> (env2, if x' >= show y' then IntAST 1 else IntAST 0)
        (IntAST x', SymbolAST y') -> (env2, if show x' >= y' then IntAST 1 else IntAST 0)
        _ -> (env2, DeadLeafAST)

compareNotEqualAST :: Environment -> AST -> AST -> (Environment, AST)
compareNotEqualAST env x y = evalNotEqualAST env x y

evalNotEqualAST :: Environment -> AST -> AST -> (Environment, AST)
evalNotEqualAST env x y =
    let (env1, evaluatedX) = evalAST env x
        (env2, evaluatedY) = evalAST env1 y
    in case (evaluatedX, evaluatedY) of
        (IntAST x', IntAST y') -> (env2, if x' /= y' then IntAST 1 else IntAST 0)
        (SymbolAST x', SymbolAST y') -> (env2, if x' /= y' then IntAST 1 else IntAST 0)
        (SymbolAST x', IntAST y') -> (env2, if x' /= show y' then IntAST 1 else IntAST 0)
        (IntAST x', SymbolAST y') -> (env2, if show x' /= y' then IntAST 1 else IntAST 0)
        _ -> (env2, DeadLeafAST)

-- * --------------------------------- MATH ---------------------------------- * --

addAST :: Environment -> AST -> AST -> (Environment, AST)
addAST env x y = 
    let (env1, evaluatedX) = evalAST env x
        (env2, evaluatedY) = evalAST env1 y
    in case (evaluatedX, evaluatedY) of
        (IntAST x', IntAST y') -> (env2, IntAST (x' + y'))
        (SymbolAST x', SymbolAST y') -> (env2, IntAST ((read x') + (read y')))
        (SymbolAST x', IntAST y') -> (env2, IntAST ((read x') + y'))
        (IntAST x', SymbolAST y') -> (env2, IntAST (x' + (read y')))
        _ -> (env2, DeadLeafAST)


subAST :: Environment -> AST -> AST -> (Environment, AST)
subAST env x y = evalSubAST env x y

evalSubAST :: Environment -> AST -> AST -> (Environment, AST)
evalSubAST env x y =
    let (env1, evaluatedX) = evalAST env x
        (env2, evaluatedY) = evalAST env1 y
    in case (evaluatedX, evaluatedY) of
        (IntAST x', IntAST y') -> (env2, IntAST (x' - y'))
        (SymbolAST x', SymbolAST y') -> (env2, IntAST ((read x') - (read y')))
        (SymbolAST x', IntAST y') -> (env2, IntAST ((read x') - y'))
        (IntAST x', SymbolAST y') -> (env2, IntAST (x' - (read y'))) 
        _ -> (env2, DeadLeafAST)


mulAST :: Environment -> AST -> AST -> (Environment, AST)
mulAST env x y = evalMulAST env x y

evalMulAST :: Environment -> AST -> AST -> (Environment, AST)
evalMulAST env x y =
    let (env1, evaluatedX) = evalAST env x
        (env2, evaluatedY) = evalAST env1 y
    in case (evaluatedX, evaluatedY) of
        (IntAST x', IntAST y') -> (env2, IntAST (x' * y'))
        (SymbolAST x', SymbolAST y') -> (env2, IntAST ((read x') * (read y')))
        (SymbolAST x', IntAST y') -> (env2, IntAST ((read x') * y'))
        (IntAST x', SymbolAST y') -> (env2, IntAST (x' * (read y')))
        _ -> (env2, DeadLeafAST)


divAST :: Environment -> AST -> AST -> (Environment, AST)
divAST env x y = evalDivAST env x y

evalDivAST :: Environment -> AST -> AST -> (Environment, AST)
evalDivAST env x y =
    let (env1, evaluatedX) = evalAST env x
        (env2, evaluatedY) = evalAST env1 y
    in case (evaluatedX, evaluatedY) of
        (IntAST x', IntAST y') -> (env2, if y' /= 0 then IntAST (x' `div` y') else DeadLeafAST)
        (SymbolAST x', SymbolAST y') -> (env2, IntAST ((read x') `div` (read y')))
        (SymbolAST x', IntAST y') -> (env2, IntAST ((read x') `div` y'))
        (IntAST x', SymbolAST y') -> (env2, IntAST (x' `div` (read y')))
        _ -> (env2, DeadLeafAST)


modAST :: Environment -> AST -> AST -> (Environment, AST)
modAST env x y = evalModAST env x y

evalModAST :: Environment -> AST -> AST -> (Environment, AST)
evalModAST env x y =
    let (env1, evaluatedX) = evalAST env x
        (env2, evaluatedY) = evalAST env1 y
    in case (evaluatedX, evaluatedY) of
        (IntAST x', IntAST y') -> (env2, if y' /= 0 then IntAST (x' `mod` y') else DeadLeafAST)
        (SymbolAST x', SymbolAST y') -> (env2, IntAST ((read x') `mod` (read y')))
        (SymbolAST x', IntAST y') -> (env2, IntAST ((read x') `mod` y'))
        (IntAST x', SymbolAST y') -> (env2, IntAST (x' `mod` (read y')))
        _ -> (env2, DeadLeafAST)


-- * --------------------------------- EVAL ---------------------------------- * --


evalAST :: Environment -> AST -> (Environment, AST)
evalAST env expr = trace ("Evaluating expression: " ++ show expr ++ " in env: " ++ show env) $ case expr of
    AST [] -> (env, DeadLeafAST)

    AST (SymbolAST "=" : x : y : _) -> let (_, resultAST) = compareEqualAST env x y in (env, resultAST)
    AST (SymbolAST ">" : x : y : _) -> let (_, resultAST) = compareGreaterAST env x y in (env, resultAST)
    AST (SymbolAST "<" : x : y : _) -> let (_, resultAST) = compareLessAST env x y in (env, resultAST)
    AST (SymbolAST "<=" : x : y : _) -> let (_, resultAST) = compareLessEqualAST env x y in (env, resultAST)
    AST (SymbolAST ">=" : x : y : _) -> let (_, resultAST) = compareGreaterEqualAST env x y in (env, resultAST)
    AST (SymbolAST "!=" : x : y : _) -> let (_, resultAST) = compareNotEqualAST env x y in (env, resultAST)
    AST (SymbolAST "+" : x : y : _) -> let (_, resultAST) = addAST env x y in (env, resultAST)
    AST (SymbolAST "-" : x : y : _) -> let (_, resultAST) = subAST env x y in (env, resultAST)
    AST (SymbolAST "*" : x : y : _) -> let (_, resultAST) = mulAST env x y in (env, resultAST)
    AST (SymbolAST "/" : x : y : _) -> let (_, resultAST) = divAST env x y in (env, resultAST)
    AST (SymbolAST "%" : x : y : _) -> let (_, resultAST) = modAST env x y in (env, resultAST)

    IfAST cond expr1 expr2 -> trace "Evaluating IfAST..." $
        let (env1, resultCond) = evalAST env cond
        in trace ("Condition result: " ++ show resultCond) $
            case resultCond of
                IntAST 0 -> evalAST env1 expr2
                IntAST _ -> evalAST env1 expr1
                _ -> (env1, DeadLeafAST)

    DefineAST name expr -> trace ("Defining: " ++ name) $
        let (newEnv, value) = evalAST env expr
        in ((name, value) : newEnv, value)

    LambdaAST paramsAST body -> trace "Creating LambdaClosure..." $
        let params = extractArgs paramsAST
        in (env, LambdaClosure params body env)



    AST (x:xs) -> trace "Evaluating AST sequence..." $ 
        case x of
            SymbolAST funcName -> 
                case lookup funcName env of
                    Just (LambdaClosure paramNames body closureEnv) ->
                        let argValues = map (\argExpr -> snd (evalAST env argExpr)) xs
                            extendedEnv = zip paramNames argValues ++ closureEnv
                        in trace ("Extended environment: " ++ show extendedEnv) $
                            evalAST extendedEnv body
                    _ -> evalASTSequence env (x:xs)
            _ -> evalASTSequence env (x:xs)

    SymbolAST x -> trace ("Looking up: " ++ x) $
        case lookup x env of
            Just val -> (env, val)
            Nothing -> (env, SymbolAST x)



    IntAST x -> (env, IntAST x)
    _ -> (env, DeadLeafAST)

evalASTSequence :: Environment -> [AST] -> (Environment, AST)
evalASTSequence env [] = (env, DeadLeafAST)
evalASTSequence env [x] = evalAST env x
evalASTSequence env (x:xs) =
    let (newEnv, _) = evalAST env x
    in evalASTSequence newEnv xs

extractArgs :: AST -> [String]
extractArgs (AST []) = []
extractArgs (AST (SymbolAST x : xs)) = x : extractArgs (AST xs)
extractArgs _ = []

traceLittleMsg :: String -> AST -> (Environment, AST) -> (Environment, AST)
traceLittleMsg msg arg (env, result) =
    trace (msg ++ " " ++ printAST arg ++ " => " ++ printAST result) (env, result)

traceMsg :: String -> AST -> [AST] -> (Environment, AST) -> (Environment, AST)
traceMsg msg arg _ (env, result) =
    trace (msg ++ " " ++ printAST arg ++ " => " ++ printAST result) (env, result)

