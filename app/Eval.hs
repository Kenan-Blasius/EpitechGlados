module Eval (evalAST) where

import System.Environment
import Types
import Debug.Trace

evalAST :: AST -> AST
evalAST (AST []) = DeadLeafAST
evalAST (AST (SymbolAST "=" : x : y : _)) =
    trace ("SymbolAST =: " ++ show x ++ " " ++ show y) $
    case (evalAST x, evalAST y) of
        (IntAST x', IntAST y') ->
            trace ("SymbolAST =: " ++ show x' ++ " " ++ show y') $
            if x' == y' then IntAST 1 else IntAST 0
        (SymbolAST x', SymbolAST y') ->
            trace ("SymbolAST =: " ++ show x' ++ " " ++ show y') $
            if x' == y' then IntAST 1 else IntAST 0
        (SymbolAST x', IntAST y') ->
            trace ("SymbolAST =: " ++ show x' ++ " " ++ show y') $
            if x' == show y' then IntAST 1 else IntAST 0
        (IntAST x', SymbolAST y') ->
            trace ("SymbolAST =: " ++ show x' ++ " " ++ show y') $
            if show x' == y' then IntAST 1 else IntAST 0
        _ -> trace ("SymbolAST =: " ++ show x ++ " " ++ show y) DeadLeafAST
evalAST (AST (SymbolAST ">" : x : y : _)) =
    trace ("SymbolAST =: " ++ show x ++ " " ++ show y) $
    case (evalAST x, evalAST y) of
        (IntAST x', IntAST y') ->
            trace ("SymbolAST =: " ++ show x' ++ " " ++ show y') $
            if x' > y' then IntAST 1 else IntAST 0
        (SymbolAST x', SymbolAST y') ->
            trace ("SymbolAST =: " ++ show x' ++ " " ++ show y') $
            if x' > y' then IntAST 1 else IntAST 0
        (SymbolAST x', IntAST y') ->
            trace ("SymbolAST =: " ++ show x' ++ " " ++ show y') $
            if x' > show y' then IntAST 1 else IntAST 0
        (IntAST x', SymbolAST y') ->
            trace ("SymbolAST =: " ++ show x' ++ " " ++ show y') $
            if show x' > y' then IntAST 1 else IntAST 0
        _ -> trace ("SymbolAST =: " ++ show x ++ " " ++ show y) DeadLeafAST
evalAST (AST (x:xs)) =
    trace ("AST: " ++ show x) $ evalAST x
evalAST (IfAST cond expr1 expr2) =
    trace ("IfAST: Condition = " ++ printAST cond) $
    case evalAST cond of
        IntAST 0 -> trace "IfAST: Condition is 0, evaluating expr2" $ evalAST expr2
        IntAST _ -> trace "IfAST: Condition is non-zero, evaluating expr1" $ evalAST expr1
        _ -> trace "IfAST: Condition is not an integer, returning DeadLeafAST" DeadLeafAST
evalAST (DefineAST name expr) =
    trace ("Defining: " ++ name) $ evalAST expr
evalAST (LambdaAST args body) =
    trace ("Lambda: " ++ printAST args) $ evalAST body
evalAST (IntAST x) = IntAST x
evalAST (SymbolAST x) =
    trace ("Symbol: " ++ x) DeadLeafAST
