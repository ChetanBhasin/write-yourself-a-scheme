module SchemeEvaluator where

import SchemeParser (
    LispVal (String, Atom, Number, Bool, List, DottedList),
    parseExpr)
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Error

-- Defines the types of errors for our Scheme
data LispError = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

-- Implements string conversions for LispError type
showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message function) = message ++ ": " ++ show function
showError (NumArgs expected found) = "Expected: " ++ show expected ++ " args; "
    ++ "found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
    ++ ", found " ++ show found
showError (Parser parseErr) = "Parser error at " ++ show parseErr

-- Implements the typeclass Show for LispError
instance Show LispError where show = showError

-- Implements the typeclass Error for LispError
instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

-- Throwable error type for our Scheme
type ThrowsError = Either LispError

-- Helper function to trap error based actions
trapError action = catchError action (return . show)

-- Lifts the error value from Either for ThrowsError type
extractValue :: ThrowsError a -> a
extractValue (Right val) = val


-- Helper function for unwording list of LispVal
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- Implements the show for LispVal
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

-- Show typeclass for LispVal implemented here
instance Show LispVal where show = showVal

-- Evaluation primitives for Scheme
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- Function application for Scheme
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

-- Primitive functions for Scheme
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]

-- Define binary operations on Numbers
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

-- Numarical unpacker
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed  = reads n in
                           if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- Function for parsing expressions
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
