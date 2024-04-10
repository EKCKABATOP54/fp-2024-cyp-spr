module Expr.Eval where

import Expr.StateDemo
import Expr.Expr
import Expr.Error
import qualified Data.Map.Strict as M


eval :: (Floating a, Ord a) => Expr a -> State (M.Map String a) (Either (Error a) a)
eval expr = case expr of 
    Const c -> return $ Right c
    SquareRoot e -> do 
        ev <- eval e
        return $ perfOp (\_ v -> if v >=0 then Right (sqrt v) else Left (RootOfNegative e)) ev ev
    Var v -> do
        state <- get
        return $ case M.lookup v state of 
            Just val -> Right val
            Nothing -> Left $ UndefinedVariable v
    Binop op e1 e2 -> do 
        v1 <- eval e1
        v2 <- eval e2
        return $ (
            case op of 
            Plus -> perfOp (\ x y -> Right $ x+y ) v1 v2
            Minus -> perfOp (\ x y -> Right $ x-y ) v1 v2
            Mult -> perfOp (\ x y -> Right $ x*y ) v1 v2
            Div -> perfOp (\ x y -> if y /= 0 then Right (x/y) else Left (DivisionByZero e1 e2)) v1 v2
            Pow -> perfOp (\ x y -> Right $ x** y ) v1 v2)
                
        
    
    where
        perfOp op e1 e2 = case (e1, e2) of
                    (Right v1, Right v2) -> op v1 v2
                    (Left er, _) -> Left er
                    (_, Left er) -> Left er
        perfOp :: (a -> a -> Either (Error a) a) -> Either (Error a) a -> Either (Error a) a -> Either (Error a) a

