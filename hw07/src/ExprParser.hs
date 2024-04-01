{-# LANGUAGE InstanceSigs #-}
module ExprParser where
    import Expr
    import Data.Char ( isAlpha, isAlphaNum, isDigit, digitToInt, isSpace )
    import Control.Applicative (Alternative, empty, (<|>), many, some)

    keywords :: [String]
    keywords = ["if", "then", "else", "sqrt"]


    -- It's not clear how to compose the parsers above, so people usually use a different abstraction for a parser. 
    -- A parser consumes the prefix of an input String while it's a valid string of the language being parsed. 
    -- Then the unread suffix is returned along with the result of the parsing. 
    -- The result may be a string (for identifiers), an integer (for numbers), 
    -- some algebraic data type for more complex langugaes (for example, Expr for expressions), 
    -- or even a function. 
    newtype Parser a = Parser { runParser :: String -> Maybe (String, a)}

    -- This abstraction of a parser is a Functor, which allows us to transform the parser's results. 
    instance Functor Parser where 
        fmap :: (a -> b) -> Parser a -> Parser b
        fmap f p = Parser $ \input -> 
            case runParser p input of 
            Nothing -> Nothing 
            Just (suff, r) -> Just (suff, f r) 
            
    -- The parser is also an applicative functor, which simplifies composition.       
    instance Applicative Parser where
        pure :: a -> Parser a
        pure res = Parser $ \str -> Just (str, res)
        
        (<*>) :: Parser (a -> b) -> Parser a -> Parser b
        (<*>) f p = Parser $ \str ->
            case runParser f str of
            Just (str', f') ->
                case runParser p str' of
                Just (str'', a) -> Just (str'', f' a)
                Nothing -> Nothing
            Nothing -> Nothing
        
    -- Monadic bind is something which expresses the idea of sequential parser application. 
    -- First parse the string with this parser, and then parse the rest with that parser.  
    -- This is one of two most important operations on parsers.    
    instance Monad Parser where
        (>>=) :: Parser a -> (a -> Parser b) -> Parser b
        (>>=) f p = Parser $ \str ->
            case runParser f str of
                Just (str', res) -> runParser (p res) str'
                Nothing -> Nothing

    -- Alternative operation allows us to express that something is either this or that. 
    -- Note that it favors the left-hand parser: if it succeeds consuming any prefix of the input string, 
    -- the right parser will not be tried. 

    
    instance Alternative Parser where
        empty :: Parser a
        empty = Parser $ const Nothing -- a parser which always reports an error: no strings in its language.

        (<|>) :: Parser a -> Parser a -> Parser a
        (<|>) l r = Parser $ \str ->
            case runParser l str of
            Just (str', res) -> Just (str', res)
            Nothing -> runParser r str
    

    -- This function creates a parser which checks that a predicate holds for the first character of an input string.  
    satisfy :: (Char -> Bool) -> Parser Char 
    satisfy p = Parser $ \str -> 
        case str of 

            (h:t) | p h -> Just (t, h) 
            _ -> Nothing

    parseInteger :: Num a => Parser (Expr a)
    parseInteger = do 
        digits <- some $ satisfy isDigit
        return $ fromInteger $ toInteger (foldl (\n d -> n*10 + d) 0 (map digitToInt digits))

    parseIdent :: Parser String
    parseIdent = do
        h <- satisfy isAlpha
        t <- many $ satisfy isAlphaNum
        return (h:t)
    
    parseVariable :: Parser (Expr a)
    parseVariable = do 
        word <- parseIdent
        if elem word keywords 
            then Parser $ \_ -> Nothing
            else return $ Var word

    parseSqrt :: Num a => Parser (Expr a)
    parseSqrt = do 
        ident <- parseIdent
        case ident of 
            "sqrt" -> do
                some $ satisfy isSpace
                innerExpr <- parseExpr
                return $ SquareRoot innerExpr
            _ -> Parser $ \_ -> Nothing

    parseBinaryOp  :: Num a => Parser (Expr a)
    parseBinaryOp = do
        op <- binopChar 
        some $ satisfy isSpace
        e1 <- parseExpr
        some $ satisfy isSpace
        e2 <- parseExpr
        return $ op e1 e2
        where
            binopChar = do 
                opChar <- satisfy $ \c -> c `elem` "+-*/^"
                return (case opChar of
                    '+' -> Binop Plus
                    '-' -> Binop Minus
                    '*' -> Binop Mult
                    '/' -> Binop Div
                    '^' -> Binop Pow)



    parseExpr :: Num a =>  Parser (Expr a)
    parseExpr = parseInteger <|> parseVariable <|> parseSqrt <|> parseBinaryOp
