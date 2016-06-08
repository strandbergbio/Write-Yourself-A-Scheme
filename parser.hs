-- Need to add support for escaped characters inside string (?)
-- Need to add support for vectors (Exercise 2 at bottom of parsing page)

{-# LANGUAGE ExistentialQuantification #-}

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import System.IO
import Data.IORef
import Control.Monad
import Control.Monad.Except
import Numeric
import Data.Char (digitToInt)

main :: IO ()
main = do args <- getArgs
          if null args
             then runRepl
             else runOne $ args

-- Building a REPL
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = 
    runIOThrows $ liftM show $ ((liftThrows $ readExpr expr) >>= eval env)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= 
         flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
  >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primitiveBindings 
          >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint
------------

----------- Mutable State \begin\
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef 
                     >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
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
       writeIORef envRef ((var, valueRef):env)
       return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = 
    readIORef envRef >>= extendEnv bindings >>= newIORef
        where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
              addBinding (var, value) = do ref <- newIORef value
                                           return (var, ref)

primitiveBindings :: IO Env
primitiveBindings = 
    nullEnv >>= (flip bindVars (map (makeFunc IOFunc) ioPrimitives ++
                                map (makeFunc PrimitiveFunc) primitives))
    where makeFunc constructor (var, func) = (var, constructor func)

makeFunc :: (Maybe String) -> Env -> [LispVal] -> 
            [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = 
    return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

------------- Mutable State \end\


----------------IO Primitives \begin\
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename


----------------IO Primitives \end\

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of 
                             Left err  -> throwError $ Parser err
                             Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr
readExprList = readOrThrow (endBy parseExpr spaces)

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
	| List [LispVal]
	| DottedList [LispVal] LispVal
	| Number Int
        | Float Float
	| String String
	| Bool Bool
        | Character Char
        | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
        | Func { params :: [String], vararg :: (Maybe String),
                 body :: [LispVal], closure :: Env }
        | IOFunc ([LispVal] -> IOThrowsError LispVal)
        | Port Handle

instance Show LispVal where show = showVal

parseExpr :: Parser LispVal
parseExpr = parseQuoted
            <|> parseString 
            <|> try parseCharacter
            <|> try parseNumber
            <|> parseAtom
            <|> do char '('
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x

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

parseSpace :: Parser LispVal
parseSpace = do
  (try (string "#\\space")) <|> (string "#\\")
  eof
  return $ Character ' '

parseNewline :: Parser LispVal
parseNewline = do
  string "#\\newline"
  eof
  return $ Character '\n'

parseCharacter :: Parser LispVal
parseCharacter = (try parseSpace) <|> (try parseNewline)
                 <|> do string "#\\"
                        x <- anyChar
                        eof 
                        return $ Character x

parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many ((noneOf "\"") <|> escaped)
	char '"'
	return $ String x

-- needs to be fixed to ouput correctly on different escapes
escaped :: Parser Char
escaped = char ('\\') >> (oneOf "nrt\\\"")

testParser :: Parser LispVal
testParser = do
  x <- many ((char '\\') >> anyChar)
  char 'b'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
	first <- letter <|> symbol
	rest <- many (letter <|> digit <|> symbol)
	let atom = first:rest
	return $ case atom of
		"#t" -> Bool True
		"#f" -> Bool False
		_    -> Atom atom

parseFloat :: Parser LispVal
parseFloat = do
  nums1 <- many1 digit
  char '.'
  nums2 <- many1 digit
  eof
  return $ Float $ fst $ (readFloat (nums1 ++ "." ++ nums2)) !! 0

-- Can handle integers of different bases, not floats
parseNumber :: Parser LispVal
parseNumber = try ((many1 digit) >>= (return . Number . read)) 
    <|> parseFloat
    <|> parseNumberBase
    where parseNumberBase = do 
          char '#'
          base <- (oneOf "bodx")
          nums <- case base of
             'b' -> many1 (oneOf "01")
             'o' -> many1 (oneOf "01234567")
             'd' -> many1 (oneOf "0123456789")
             'x' -> many1 (oneOf "0123456789ABCDEF")
          eof
          return $ case base of
             'b' -> Number $ fst $ (readBin nums) !! 0
             'o' -> Number $ fst $ (readOct nums) !! 0
             'd' -> Number $ fst $ (readDec nums) !! 0
             'x' -> Number $ fst $ (readHex nums) !! 0
             where readBin = readInt 2 (\x -> elem x ['0','1']) digitToInt





--------------------------------------------------------

showVal :: LispVal -> String
showVal (String contents)      = "\"" ++ contents ++ "\""
showVal (Character '\n')       = "#\\newline"
showVal (Character ' ')        = "#\\space"
showVal (Character contents)   = "#\\" ++ [contents]
showVal (Atom name)            = name
showVal (Number contents)      = show contents
showVal (Float contents)       = show contents
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"
showVal (List contents)        = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . "
                                 ++ showVal tail ++ ")"
showVal (PrimitiveFunc _)      = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = 
    "(lambda (" ++ unwords (map show args) ++
    (case varargs of
       Nothing -> ""
       Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _)               = "<IO port>"
showVal (IOFunc _)             = "<IO primitive>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- need to eval DottedList (???)
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "load", String filename]) =
    load filename >>= liftM last . mapM (eval env)
eval env (List [Atom "if", pred, conseq, alt]) = 
    do result <- eval env pred
       case result of 
            Bool False -> eval env alt
            Bool True  -> eval env conseq
            otherwise  -> throwError $ TypeMismatch "Boolean in (eval if)" result
eval env (List [Atom "set!", Atom var, form]) = 
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = 
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs :body)) = 
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = 
    makeVarArgs varargs env [] body
eval env (List (function : args))  = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval env (List x)                   = mapM (eval env) x >>= (return . List)
eval env (Atom id)                  = getVar env id
eval env val@(DottedList _ _)       = return val 
eval env val@(Number _)             = return val
eval env val@(Float _)              = return val
eval env val@(Bool _ )              = return val
eval env val@(Character _)          = return val
eval env val@(String _)             = return val
-- eval badform = throwError $ BadSpecialForm "Unrecognized special form" badform


apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (IOFunc func) args        = func args
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args = 
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args)
            >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
                                  Just argName -> liftIO $ bindVars env 
                                                  [(argName, List $ remainingArgs)]
                                  Nothing -> return env

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),
              ("cond", cond),
              ("case", caseLisp),
              ("symbol?", symbolQ),
              ("string?", stringQ),
              ("number?", numberQ),
              ("boolean?", booleanQ),
              ("make-string", makeString),
              ("string-length", stringLength),
              ("string-ref", stringRef)]
              --("symbol->string", symbolToString . head)
              
numericBinop :: (Int -> Int -> Int) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params 
                                >>= return . Number . foldl1 op

-- can't read strings in different bases yet
unpackNum :: LispVal -> ThrowsError Int
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n :: [(Int, String)] in
                       if null parsed
                          then throwError $ TypeMismatch "number" $ String n
                          else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

stringQ :: [LispVal] -> ThrowsError LispVal
stringQ [(String _)] = return $ Bool True
stringQ [_]          = return $ Bool False
stringQ other        = throwError $ NumArgs 1 other

symbolQ :: [LispVal] -> ThrowsError LispVal
symbolQ [(Atom _)]   = return $ Bool True
symbolQ [_]          = return $ Bool False 
symbolQ other        = throwError $ NumArgs 1 other

numberQ :: [LispVal] -> ThrowsError LispVal
numberQ [(Number _)] = return $ Bool True
numberQ [_]          = return $ Bool False
numberQ other        = throwError $ NumArgs 1 other

booleanQ :: [LispVal] -> ThrowsError LispVal
booleanQ [(Bool _)] = return $ Bool True
booleanQ [_]        = return $ Bool False
booleanQ other      = throwError $ NumArgs 1 other

listQ :: [LispVal] -> ThrowsError LispVal
listQ [(List _)] = return $ Bool True
listQ [_]        = return $ Bool False
listQ other      = throwError $ NumArgs 1 other

-- make this pass error when not given an Atom
symbolToString :: LispVal -> LispVal
symbolToString (Atom str) = String str
symbolToString _          = String ""

-- also make this error
stringToSymbol :: LispVal -> LispVal
stringToSymbol (String str) = Atom str
stringToSymbol _            = Atom ""

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool)
             -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s)    = return s
unpackStr other         = return $ show other

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b)      = return b
unpackBool (String "#t") = return True
unpackBool (String "#f") = return False
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool 

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)]         = return x
car [DottedList (x:xs) _] = return x
car [badArg]              = throwError $ TypeMismatch "pair" badArg
car badArgList            = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgList            = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []]            = return $ List [x1]
cons [x, List xs]             = return $ List (x:xs)
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1, x2]                 = return $ DottedList [x1] x2
cons badArgList               = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]     = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]     = return $ Bool $ arg1 == arg2
eqv [(Character arg1), (Character arg2)] = return $ Bool $ arg1 == arg2
eqv [(Float arg1), (Float arg2)]   = return $ Bool $ arg1 == arg2
eqv [(List arg1), (List arg2)] =
    return $ Bool $ (length arg1 == length arg2) && 
               (all eqvPair $ zip arg1 arg2)
               where eqvPair (x1, x2) = case eqv [x1, x2] of
                                          Left err -> False
                                          Right (Bool val) -> val
eqv [(DottedList xs x), (DottedList ys y)] = 
    eqv [(List (xs ++ [x])), (List (ys ++ [y]))]
eqv [_,_]      = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList



---------------------------------------------------------
-- Part 3: Error Checking and Exceptions

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
instance Show LispError where show = showError

type ThrowsError = Either LispError

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                          ++ "args; found values "
                                          ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected 
                                          ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr



trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val



---------------------------------------
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
    (do unpacked1 <- unpacker arg1
        unpacked2 <- unpacker arg2 
        return $ unpacked1 == unpacked2) `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = 
    do primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) 
                                          [AnyUnpacker unpackStr,
                                           AnyUnpacker unpackNum,
                                           AnyUnpacker unpackBool]
       eqvEquals <- eqv [arg1, arg2]
       return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- equalTest = [x | (Right x) <- mapM (unpackEquals (String "#t") (Bool True)) [AnyUnpacker unpackStr, AnyUnpacker unpackNum, AnyUnpacker unpackBool]]

eqvList [(List arg1), (List arg2)] = (mapM equal (zipWith (\x y -> [x,y]) arg1 arg2)) >>= orLisp
eqvList [arg1, arg2] = eqv [arg1, arg2]

orLisp :: [LispVal] -> ThrowsError LispVal
orLisp []                = return $ Bool False
orLisp ((Bool True):xs)  = return $ Bool True
orLisp ((Bool False):xs) = orLisp xs
orLisp other= throwError $ TypeMismatch "expected Bool LispVal for orLisp" (List other)





data Conditional = forall a. Conditional (a -> ThrowsError Bool)


-- This probably doesn't work because it doesn't accept bound variables
cond :: [LispVal] -> ThrowsError LispVal
cond ((List ((Bool b):xs)):rest) = if b
                                   then return (last xs)
                                   else cond rest
cond ((Atom "else"):rest)        = return (last rest)
cond x = throwError $ BadSpecialForm "Cond clauses incorrectly formatted" (List x)


-- PROBABLY DOESN'T WORK
caseLisp :: [LispVal] -> ThrowsError LispVal
caseLisp ((List dat):expr) = 
  mapM eqv [[d, e] | d <- dat, e <- expr] >>= (return . List)
caseLisp ((Atom "else"):expr) = return $ List expr
caseLisp x = throwError $ BadSpecialForm "case clause incorrectly formatted" (List x)


makeString :: [LispVal] -> ThrowsError LispVal
makeString ((Number k):[Character char]) = return $ String $ take k (repeat char)
makeString ((Number k):[String str])     = return $ String $ take k (repeat (head str))
makeString [Number k]                    = return $ String $ take k (repeat ' ')
makeString other = throwError $ BadSpecialForm "make-string wrong inputs" (List other)

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [String str] = return $ Number $ length str
stringLength other = throwError $ BadSpecialForm "string-length wrong inputs" (List other)

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [String str, Number k] = return $ String [str !! k]
stringRef other = throwError $ BadSpecialForm "string-ref wrong inputs" (List other)




