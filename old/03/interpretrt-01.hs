module Main where
import System.Environment
import Text.ParserCombinators.Parsec 
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array
instance Show LispVal where show = showVal

data LispVal = Atom String
             | List [ LispVal ]
             | DottedList [ LispVal ] LispVal
             | String String
             | Charactor Char
             | Bool Bool
             | Number Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Vector (Array Int LispVal)
               
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
-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = do x <- many1 digit
--                 return $ Number $ read x
-- parseNumber = return . Number . read =<< many1 digit
parseNumber = do num <- parseDicimal1
                        <|> parseDicimal2
                        <|> parseHex
                        <|> parseOct
                        <|> parseBin
                 return num

parseDicimal1 :: Parser LispVal
parseDicimal1 = do x <- many1 digit
                   (return . Number . read ) x

parseDicimal2 :: Parser LispVal
parseDicimal2 = do try $ string "#d"
                   return . Number . read =<< many1 digit

parseHex :: Parser LispVal
parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x) 

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

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

  
spaces1 :: Parser ()
spaces1 = skipMany1 space

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
  
parseList :: Parser LispVal
parseList = do char '(' >> spaces
               head <- sepEndBy parseExpr spaces1
               ( do char '.' >> spaces1
                    tail <- parseExpr
                    spaces >> char ')'
                    return $ DottedList head tail
                 ) <|> (spaces >> char ')' >> (return $ List head))
                       
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Charactor contents) = show contents
showVal (Ratio contents) = show contents
showVal (Complex contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val


eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Bool _) = val
eval val@(Charactor _) = val
eval val@(Ratio _) = val
eval val@(Complex _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("reminder", numericBinop rem),
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp)]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

symbolp, numberp, stringp, boolp, listp :: LispVal -> LispVal
symbolp (Atom _) = Bool True
stmbolp _        = Bool False
numberp (Number _) = Bool True
numberp _ = Bool False
stringp (String _) = Bool True
stringp _ = Bool False
boolp (Bool _) = Bool True
boolp _ = Bool False
listp (List _) = Bool True
listp (DottedList _ _) = Bool True
listp _ = Bool False



numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0
              
symbol2string, string2symbol :: LispVal -> LispVal
symbol2string (Atom s) = String s
symbol2string _ = String ""
string2symbol (String s) = Atom s
string2symbol _ = Atom ""


main :: IO ()
main = do
  getArgs >>= print . eval . readExpr . head
