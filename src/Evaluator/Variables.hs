module Evaluator.Variables where 

import Data.IORef -- IORef module enables for stateful variables in IO monad

-- Env is a stateful variable (IORef) that holds a map of strings -> IORef LispVal
type Env = IORef[(String, IORef LispVal)]

-- Since IORefs can only be used within IO 
-- |nullEnv creates an empty environment
nullEnv :: IO Env
nullEnv = newIORef []

-- Needs an Error Monad to handle errors like unbound variables
-- ErrorT is a monad transformer that layers error handling on top of IO
-- May contain IO actions and throws a LispError
-- Curried type constructor that still accepts an argument: return type of the function
type IOThrowsError = ErrorT LispError IO
