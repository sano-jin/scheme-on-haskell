module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric

data LispVal = Atom String
             | List [ LispVal ]
             | DottedList [ LispVal ] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Charactor Char

parseCharactor :: Parser LispVal
parseCharactor = do try $ string "#\\"
                    x <- 
  

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
-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = do x <- many1 digit
--                 return $ Number $ read x
-- parseNumber = return . Number . read =<< many1 digit
parseNumber = do num <- parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin
                 return num


parseDigital1 :: Parser LispVal
parseDigital1 = do x <- many1 digit
                   (return . Number . read ) x

parseDigital2 :: Parser LispVal
parseDigital2 = do try $ string "#d"
                   return . Number . read =<< many1 digit

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x) 

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ NUmber (oct2dig x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

oct2dig x = fst $ Numeric.readOct x !! 0
hex2dig x = fst $ Numeric.readHex x !! 0
bin2dig x = bin2dig' 0 x
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                           bin2dig' old xs



parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseBool
            <|> parseCharactor

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

parseBool :: Parser LispVal
parseBool = do string "#"
               x <- oneOf "tf"
               return $ case x of
                          't' -> Bool True
                          'f' -> Bool False
                      
spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"



main :: IO ()
main = do
  args <- getArgs
  putStrLn (readExpr (args !! 0))
