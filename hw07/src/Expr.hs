--{-# LANGUAGE GADTs #-}

module Expr where 

  import Text.Printf (printf)

  data Expr a = Const a 
        | SquareRoot (Expr a)
        | Var String
        | Binop BinOptor (Expr a) (Expr a)
        deriving Eq
 
  {-
  data Expr a where
    Const :: Num a => a -> Expr a
    SquareRoot :: Num a => Expr a -> Expr a
    Binop :: Num a => BinOptor -> Expr a -> Expr a -> Expr a
    Var :: String -> Expr a
  -}

  data BinOptor = Plus | Minus | Mult | Div | Pow deriving Eq

  instance Num a => Num (Expr a) where
    (+) = Binop Plus
    (*) = Binop Mult
    negate = Binop Minus (Const 0) 
    fromInteger = Const . fromInteger

  
  instance Show BinOptor where
    show Plus = "+"
    show Minus = "-"
    show Mult = "*"
    show Div = "/"
    show Pow = "^"

  instance Show a => Show (Expr a) where 
    show e = case e of
            (Const c)       -> show c
            (SquareRoot e1)  -> printf "sqrt(%s)" (show e1)
            (Binop op e1 e2) -> printf "(%s %s %s)" (show e1) (show op) (show e2)
            (Var v)         -> printf "%s" (show v)