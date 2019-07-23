module LispParser.Number where

import Numeric
import Data.Ratio
import Data.Complex
import LispParser.Atom
import Text.ParserCombinators.Parsec hiding (spaces)
    

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
        otherwise -> value !! 0


parseRatio :: Parser LispVal
parseRatio = do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return $ Ratio $ (read x) % (read y)

-- |Convert a LispVal Float or Integer to Haskell Double
toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n

-- |Parse a Complex Number
parseComplex :: Parser LispVal
parseComplex = do
    realPart <- (try parseFloat <|> parseDecimal)
    char '+'
    imagPart <- (try parseFloat <|> parseDecimal)
    char 'i'
    return $ Complex (toDouble realPart :+ toDouble imagPart)