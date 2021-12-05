module Lib
    ( libMain 
    ) where

import Control.Monad
import Data.Complex
import Data.Ratio
import Data.Void
import Numeric
import System.Environment
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Types 
-------------------------------------------------------------

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             deriving (Show)

-- Parser
-------------------------------------------------------------

-- Numeric conversion helpers
oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs

toDouble :: LispVal -> Double
toDouble (Float f)  = realToFrac f
toDouble (Number n) = fromIntegral n


type Parser = Parsec Void String

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = space1 

escapedChars :: Parser Char
escapedChars = do 
    char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
        '\\' -> x
        '"'  -> x
        'n'  -> '\n'
        'r'  -> '\r'
        't'  -> '\t'

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapedChars <|> noneOf "\"\\"
    char '"'
    return $ String x

-- parses characters specified like #\a #\A #\space #\newline
parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try (string "newline" <|> string "space")
            <|> do { x <- L.charLiteral; notFollowedBy alphaNumChar; return [x] }
    return $ Character $ case value of
        "space"     -> ' '
        "newline"   -> '\n'
        _           -> head value

parseAtom :: Parser LispVal
parseAtom = do
    first <- letterChar <|> symbol 
    rest <- many (letterChar <|> digitChar <|> symbol)
    let atom = first:rest
    return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseDecimal1 :: Parser LispVal
parseDecimal1 = some digitChar >>= (return . Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do
    try $ string "#x"
    x <- some digitChar
    return $ (Number . read) x

parseHex :: Parser LispVal
parseHex = do
    try $ string "#x"
    x <- some hexDigitChar
    return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do
    try $ string "#o"
    x <- some octDigitChar
    return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do
    try $ string "#b"
    x <- some binDigitChar
    return $ Number (bin2dig x)

parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseFloat :: Parser LispVal
parseFloat = L.float >>= (return . Float)

parseRatio :: Parser LispVal
parseRatio = do
    x <- some digitChar
    char '/'
    y <- some digitChar
    return $ Ratio ((read x) % (read y))

parseComplex :: Parser LispVal
parseComplex = do
    x <- try parseFloat <|> try parseDecimal1
    char '+'
    y <- try parseFloat <|> try parseDecimal1
    return $ Complex (toDouble x :+ toDouble y)

parseExpr :: Parser LispVal
parseExpr = parseAtom 
         <|> parseString 
         <|> try parseComplex
         <|> try parseFloat
         <|> try parseRatio
         <|> try parseNumber 
         <|> try parseBool
         <|> try parseCharacter

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of 
    Left err -> "No match: " ++ errorBundlePretty err
    Right val -> "Found value: " ++ show val

libMain :: IO ()
libMain = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
