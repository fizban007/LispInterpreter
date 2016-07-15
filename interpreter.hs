module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Control.Monad.Except
import System.IO
import System.Console.Haskeline
import System.Console.Haskeline.IO
import Data.IORef

import Lisp.Err
import Lisp.LispVal
import Lisp.Elements
import Lisp.Eval

-- instance Error LispError where
--   noMsg = Default "An error has occurred!"
--   strMsg = Default

-- Individual parsers

-- escaped :: Parser LispVal
-- escaped = do
--   char '\\'
--   char '\"'
--   return $ String "\\\""
type Env = IORef [(String, IORef LispVal)]

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
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

nullEnv :: IO Env
nullEnv = newIORef []


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Float _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
     do result <- eval env pred
        case result of
             Bool False -> eval env alt
             otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalString :: Env -> String -> IO String
-- evalString expr = return $ extractValue $ trapError ( liftM show $ readExpr expr >>= eval)
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> InputT IO ()
evalAndPrint env expr = evalString env expr >>= outputStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return()
    else action result >> until_ pred prompt action

-- runRepl :: IO ()
-- runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

main :: IO ()
-- main = do
--   args <- getArgs
--   case length args of
--     0 -> runRepl
--     1 -> evalAndPrint $ args !! 0
--     _ -> putStrLn "Program only takes 0 or 1 argument!"
main = runInputT defaultSettings loop
  where
    loop :: InputT IO()
    loop = do
      minput <- getInputLine "Lisp>>> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do evalAndPrint input
                         loop
  -- evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  -- putStrLn $ extractValue $ trapError evaled
  -- let neg a = (-a)
  -- print $ eval $ (Number $ neg 3)
  -- putStrLn ("Input name: ")
  -- args <- getArgs
  -- arg2 <- head $ head getArgs
  -- putStrLn ( readExpr (args !! 0) )
