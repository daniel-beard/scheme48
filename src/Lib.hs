module Lib
    ( libMain
    ) where

import Control.Monad
import Data.Array
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
             | Vector (Array Int LispVal)
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)

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

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do
    char ','
    char '@'
    x <- parseExpr
    return $ List [Atom "unquote-splicing", x]

parseVector :: Parser LispVal
parseVector = do
    arrayValues <- sepBy parseExpr spaces
    return $ Vector (listArray (0, length arrayValues - 1) arrayValues)

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> try parseComplex
         <|> try parseFloat
         <|> try parseRatio
         <|> try parseNumber
         <|> try parseBool
         <|> try parseCharacter
         <|> parseQuoted
         <|> parseQuasiQuoted
         <|> parseUnQuote
         <|> parseUnQuoteSplicing
         <|> try (do
             string "#("
             x <- parseVector
             char ')'
             return x)
         <|> do
                char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

readExpr :: String -> LispVal 
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ errorBundlePretty err
    Right val -> val

-- Eval
-------------------------------------------------------------

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
--TODO: Implement the rest so this isn't required
showVal _ = error "showVal not implemented for type"

instance Show LispVal where show = showVal

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _)   = Bool True
symbolp _          = Bool False
numberp (Number _) = Bool True
numberp _          = Bool False
stringp (String _) = Bool True
stringp _          = Bool False
boolp   (Bool _)   = Bool True
boolp   _          = Bool False
listp   (List _)   = Bool True
listp   (DottedList _ _) = Bool False
listp   _          = Bool False

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp)]

-- lookup primitive, if not present, return a `Bool False` value
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _ ) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
--TODO: Implement the rest so this isn't required
eval _ = error "eval not implemented for type"

libMain :: IO ()
libMain = getArgs >>= print . eval . readExpr . head
