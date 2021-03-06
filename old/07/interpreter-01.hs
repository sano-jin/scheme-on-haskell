{-# LANGUAGE ExistentialQuantification #-}
module Main where
import System.Environment
import Text.ParserCombinators.Parsec 
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array
import Control.Monad.Except
import System.IO
import Data.IORef
instance Show LispVal where show = showVal
instance Show LispError where show = showError
type ThrowsError = Either LispError
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val


runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var


getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboudVar "Getting an unbound variable: " var)
                         (liftIO . readIORef)
                         (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboudVar "Setting an unboud variable: " var)
                               (liftIO . (flip writeIORef value))
                               (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
    valueRef <- newIORef value
    env <- readIORef envRef
    writeIORef envRef ((var, valueRef) : env)
    return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bidindgs env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)
       
        
                             

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

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboudVar String String
               | Default String

data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)
               
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

showError :: LispError -> String
showError (UnboudVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " arg; found values " ++ unwordsList found 
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr


trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val


eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Float _) = return val
eval env val@(Bool _) = return val
eval env val@(Charactor _) = return val
eval env val@(Ratio _) = return val
eval env val@(Complex _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  do result <- eval env pred
     case result of
       Bool True -> eval env conseq 
       Bool False -> eval env alt
       _ -> throwError $ TypeMismatch "bool" pred       
eval env (List (Atom "cond" : expr : rest)) = do
  eval' expr rest
  where eval' (List [cond, value]) (x : xs) = do
          result <- eval env cond
          case result of
            Bool False -> eval' x xs
            Bool True -> eval env value
            otherwise -> throwError $ TypeMismatch "boolean" cond
        eval' (List [Atom "else", value]) [] = do
          eval env value
        eval' (List [cond, value]) [] = do
          result <- eval env cond
          case result of
            Bool True -> eval env value
            otherwise -> throwError $ TypeMismatch "boolean" cond
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [l1@(List arg1), l2@(List arg2)] = eqvList eqv [l1, l2]
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  (do unpacked1 <- unpacker arg1
      unpacked2 <- unpacker arg2
      return $ unpacked1 == unpacked2)
  `catchError` (const $ return False)
  
equal :: [LispVal] -> ThrowsError LispVal
equal [l1@(List arg1), l2@(List arg2)] = eqvList equal [l1, l2]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList


eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [(List arg1), (List arg2)] =
  return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
  where eqvPair  (x1, x2) = case eqvFunc [x1, x2] of
                              Left error -> False
                              Right (Bool val) -> val
                                             

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                  ($ args)
                  (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("=<", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("reminder", numericBinop rem),
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp),
              ("car", car), 
              ("cdr", cdr), 
              ("cons", cons), 
              ("eq?", eqv), 
              ("eqv?", eqv), 
              ("equal?", equal)]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v

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


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                         if null parsed
                         then throwError $ TypeMismatch "number" $ String n
                         else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n  
unpackNum notNum = throwError $ TypeMismatch "number" notNum
              
symbol2string, string2symbol :: LispVal -> ThrowsError LispVal
symbol2string (Atom s) = return $ String s
symbol2string _ = return $ String ""
string2symbol (String s) = return $ Atom s
string2symbol _ = return $ Atom ""


flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action
    
runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ args !! 0
    otherwise -> putStrLn "Program takes only 0 or 1 argument"
