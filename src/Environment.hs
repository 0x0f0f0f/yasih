module Environment where 

import LispTypes

import Data.IORef
import Data.List
import Control.Monad.Except


-- Since IORefs can only be used within IO 
-- |nullEnv creates an empty environment
nullEnv :: IO Env
nullEnv = newIORef []


-- destructures the Either type and re-throws an error
-- or returns an ordinary value
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- |Run the whole top-level IOThrowsError returning an IO action
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) 
    >>= return . extractValue

-- Environment Handling

-- |Check if a variable is bound in an environment
-- Extract value from stateful var (envRef), look it up 
-- and use return to lift a Maybe monad containing True or False into an IO monad
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef 
    >>= return . maybe False (const True) . lookup var

-- |Retrieve the current value of a variable

-- Retrieve an environment then lookup a the variable in the env
-- If it is found extract its value from the IORef monad, otherwise
-- throw an UnboundVar Error
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

-- |Set a variable in an environment and return the value
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do 
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Unbound variable" var)
    -- flip switches order of arguments
        (liftIO . flip writeIORef value)
        (lookup var env)
    return value

-- |Define a variable in an environment

-- Set a variable if it is already bound, otherwise
-- create a new IO action that creates a new IORef to
-- hold the new variable, read the current environment value
-- then write back a new list consisting of the new
-- (key, value) pair cons-ed to the front of the list.
-- Then lift the whole do-lock into IOThrowsError monad
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    alreadyDef <- liftIO $ isBound envRef var
    if alreadyDef then
        setVar envRef var value >> return value
    else liftIO $ do 
        valueRef <- newIORef value 
        env <- readIORef envRef
        writeIORef envRef ((var, valueRef) : env)
        return value


-- |Extend an environment

-- |Bind multiple variables (such as in a function call)
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings =
    readIORef envRef >>= extendEnv bindings >>= newIORef
    where 
        -- Call addBinding on each member of bindings to create a list
        -- of (String, IORef LispVal) pairs and append it to the current Environment
        extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        -- take a variable name and value and create an IORef holding
        -- the new value 
        addBinding (var, value) = do 
            newVal <- newIORef value
            return (var, newVal)

        
-- |Helper functions to create function objects in IOThrowsError monad
makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing 
makeVarargs = makeFunc . Just . showVal
