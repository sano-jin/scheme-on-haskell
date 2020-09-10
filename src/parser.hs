{-# LANGUAGE ExistentialQuantification #-}
module Parser (
  LispVal(..),
  
  ) where

import System.Environment
import Text.ParserCombinators.Parsec 
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array
import Control.Monad.Except
import System.IO

-- lexer
spaces1 :: Parser ()
spaces1 = skipMany1 space

parseCharactor :: Parser LispVal
parseCharactor = do try $ string "#\\"
                    value <- try (string "newline" <|> string "space")
                             <|> do x <- anyChar
                                    notFollowedBy alphaNum
                                    return [x]
                    return $ Charactor $ case value of
                      "space" -> ' '
                      "newline" -> '\n'
                      otherwise -> (value !! 0)
  

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (noneOf "\"" <|> escapedChars)
                 char '"'
                 return $ String x

escapedChars :: Parser Char
escapedChars = do x <- char '\\' >> oneOf "\\\"nrt"
                  return $ case x of 
                             'n' -> '\n'
                             'r' -> '\r'
                             't' -> '\t'
                             _ -> x
                             
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseDicimal1
              <|> parseDicimal2
              <|> parseHex
              <|> parseOct
              <|> parseBin
              
parseDicimal1 :: Parser LispVal
parseDicimal1 =  liftM (Number . read ) many1 digit 

parseDicimal2 :: Parser LispVal
parseDicimal2 = do try $ string "#d"
                   return . Number . read =<< many1 digit

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              return . Number . hex2dig =<< many1 hexDigit

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              return . Number . oct2dig =<< many1 octDigit

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              return . Number . bin2dig =<< many1 (oneOf "10")

oct2dig x = fst $ Numeric.readOct x !! 0
hex2dig x = fst $ Numeric.readHex x !! 0
bin2dig x = bin2dig' 0 x
bin2dig' digint "" = digint
bin2dig' digint (x:xs) =
  let old = 2 * digint + (if x == '0' then 0 else 1) in
    bin2dig' old xs


parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float $ (fst . head $ readFloat (x ++ "." ++ y))

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio $ (read x) % (read y)

toDouble :: LispVal -> Double
toDouble (Float f) = f
toDouble (Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do x <- (try parseFloat <|> parseNumber)
                  char '+'
                  y <- (try parseFloat <|> parseNumber)
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseBool :: Parser LispVal
parseBool = do string "#"
               x <- oneOf "tf"
               return $ case x of
                          't' -> Bool True
                          'f' -> Bool False

-- parser
parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces1
  tail <- char '.' >> spaces1 >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [ Atom "quote", x ]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ List [ Atom "quasiquote", x ]

parseUnQuote :: Parser LispVal
parseUnQuote = do
  char ','
  x <- parseExpr
  return $ List [ Atom "unquote", x ]

parseVector :: Parser LispVal
parseVector = do arrayValues <- sepBy parseExpr spaces1
                 return $ Vector (listArray (0, (length arrayValues - 1)) arrayValues)

parseList :: Parser LispVal
parseList = do char '(' >> spaces
               head <- sepEndBy parseExpr spaces1
               ( do char '.' >> spaces1
                    tail <- parseExpr
                    spaces >> char ')'
                    return $ DottedList head tail
                 ) <|> (spaces >> char ')' >> (return $ List head))

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseQuoted
            <|> try parseNumber
            <|> try parseBool
            <|> try parseCharactor
            <|> try parseFloat
            <|> try parseRatio
            <|> try parseComplex
            <|> try parseQuasiQuoted
            <|> try parseUnQuote
            <|> try (do string "#("
                        x <- parseVector
                        char ')'
                        return x)
            <|> parseList
  
