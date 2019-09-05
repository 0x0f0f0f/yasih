module LispParser where

import LispTypes

import Data.Ratio
import Data.Complex
import Data.Array
import Data.IORef
import Numeric

import Text.ParserCombinators.Parsec hiding (spaces)

-- |Parser that recognizes one of the symbols allowed in Scheme Ident.
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

-- |Parser to ignore whitespace
spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do 
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom ( first : rest )

-- |Parse a boolean value
parseBool :: Parser LispVal
parseBool = do
    try $ char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))


-- Exercise 2.2 and 2.3
-- Parse escaped characters in strings
parseEscapedChars :: Parser Char
parseEscapedChars = do
    char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
        '\\' -> x
        '"' -> x
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _ -> x

-- |Parse a string
parseString :: Parser LispVal
parseString = do 
    char '"'
    x <- many (parseEscapedChars <|> noneOf "\"")
    char '"'
    return $ String x


-- |Parse a Number
{-  
    Parse many digits (Parser String)
    Then apply a LispVal Atom Number constructor 
    Composed with read to the resulting value
    liftM is used to promote the function to a Monad
-}
parseNumber :: Parser LispVal
parseNumber = parseDecimal 
    <|> parseDecimalExplBasis
    <|> parseBinary
    <|> parseOctal
    <|> parseHexadecimal

parseDecimal :: Parser LispVal 
parseDecimal =  many1 digit >>= (return . Number . read) 

parseDecimalExplBasis :: Parser LispVal
parseDecimalExplBasis = do 
    try $ string "#d"
    x <- many1 digit
    return $ Number (read x)

bin2dig = bin2dig' 0
bin2dig' dig "" = dig 
bin2dig' dig (x:xs) = bin2dig' (2 * dig + (if x == '0'
    then 0 else 1)) xs

parseBinary :: Parser LispVal
parseBinary = do 
    try $ string "#b"
    x <- many1 $ oneOf "01"
    return $ Number (bin2dig x)

hex2dig x = fst $ readHex x !! 0 
parseHexadecimal :: Parser LispVal
parseHexadecimal = do 
    try $ string "#x"
    x <- many1 hexDigit
    return $ Number (hex2dig x)

oct2dig x = fst $ readOct x !! 0 
parseOctal :: Parser LispVal
parseOctal = do
    try $ string "#o"
    x <- many1 octDigit
    return $ Number (oct2dig x)

parseFloat :: Parser LispVal
parseFloat = do
   x <- many1 digit
   char '.'
   y <- many1 digit
   return $ Float $ (fst . head) $ readFloat $ x++"."++y

parseCharacter :: Parser LispVal
parseCharacter = do
    string "#\\"
    value <- try (string "newline" <|> string "space")
        <|> do 
            x <- anyChar
            notFollowedBy alphaNum
            return [x]
    return $ Character $ case value of
        "space" -> ' '
        "newline" -> '\n'
        otherwise -> head value


parseRatio :: Parser LispVal
parseRatio = do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return $ Ratio $ read x % read y

-- |Convert a LispVal Float or Integer to Haskell Double
toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n

-- |Parse a Complex Number
parseComplex :: Parser LispVal
parseComplex = do
    x <- try parseFloat <|> parseDecimal
    char '+'
    y <- try parseFloat <|> parseDecimal
    char 'i'
    return $ Complex (toDouble x :+ toDouble y)

parseLisp = parse parseExpr "lisp"

-- |Parse an Expression (Either a String, a number or an Atom)
parseExpr :: Parser LispVal 
parseExpr = parseAtom
    <|> parseString
    <|> try parseRatio
    <|> try parseComplex
    <|> try parseFloat
    <|> try parseNumber
    <|> try parseBool
    <|> try parseCharacter
    <|> try parseQuoted
    <|> try parseQuasiQuoted
    <|> try parseUnQuote
    <|> try parseVector
    <|> try parseParens

-- |Parse a List of Atoms like a b c d
parseList :: Parser LispVal
parseList = sepBy parseExpr spaces >>= (return . List)

-- |Parse a Dotted list (a b c . d)
parseDottedList :: Parser LispVal
parseDottedList = do
    -- Parse a List of 0 or more expressions 
    -- Separated by spaces
    head <- endBy parseExpr spaces  
    -- Then parse the remaining Expr after the dot
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

-- |Parse a Quoted Expression 'a
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


-- |Parse a QuasiQuoted Expression
-- See https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.6
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char '`'
    x <- parseExpr
    return $ List [Atom "unquote", x]
    

-- |Parse a list expression surrounded by parens 
parseParens :: Parser LispVal
parseParens = do
    char '('
    x <- (try parseList) <|> parseDottedList
    char ')'
    return x

-- |Parse a Vector #(a b c)
-- |See https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.6
parseVector :: Parser LispVal
parseVector = do
    string "#("
    vals <- sepBy parseExpr spaces
    char ')'
    return $ 
        Vector $ listArray (0, (length vals -1)) vals

