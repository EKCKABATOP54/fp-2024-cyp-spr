module Main (main) where

import Expr
import ExprParser
import Eval
import Text.Read (readMaybe)
import Text.Printf (printf)
import StateDemo
import qualified Data.Map.Strict as M


readExpr :: IO (Expr Double)
readExpr = do 
    putStrLn "Enter expression in prefix notation"
    rawExpr <- getLine
    case runParser parseExpr rawExpr of 
        Just ("", e) -> return e
        _ -> do
            putStrLn "Error while parsing expression"
            readExpr

readMap :: IO (M.Map String Double)
readMap = do 
    putStrLn "Enter variable map as [(String, Int)]. If the list contains more than one value for the same key, the last value for the key is retained. "
    rawMap <- getLine
    let m :: Maybe [(String, Double)] 
        m = readMaybe rawMap in
            case m of 
                Nothing -> do 
                    putStrLn "Error while parsing variable map"
                    readMap
                Just v -> return $ M.fromList v


main :: IO ()
main = do 
    putStrLn "Hello"
    expr <- readExpr
    mp <- readMap
    putStrLn (show expr)
    putStrLn $ showEvalResult $ execState (eval expr) mp
    where 
        showEvalResult r = 
            case r of 
                Right v -> show v 
                Left e -> printf "Error!\n%s" (show e)