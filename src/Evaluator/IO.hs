module Evaluator.IO where 

import LispTypes
import LispParser
import Environment
import Evaluator.Operators

import Control.Monad.Except
import System.IO hiding (try)
import System.Directory 

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = 
    [("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll),
    ("print", printobj),
    ("display", printobj),
    ("println", println),
    ("newline", newline)]



-- | Wraps openFile wrapping its return value into a Port.
makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

-- | Wraps hClose wrapping its return value into a Port.
closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

-- | readProc wraps the built in hGetLine and sends the ressult to parseExpr

-- hGetLine is of type IO String but readExpr is of type String -> ThrowsError LispVal
-- So they both need to be converted (with liftIO and liftThrows) to the IOThrowsError monad
-- Only then they can be piped together by the monadic operator bind (>>=)
readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr
readProc [x] = throwError $ TypeMismatch "port" x
readProc badArgList = throwError $ NumArgs 2 badArgList

-- | writeProc converts a LispVal to a string and writes it to the specified port
-- show is called automatically since hPrint accepts a class instance of Show a
writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)
writeProc badArgList = throwError $ NumArgs 2 badArgList

-- | Display a string or a character
printobj :: [LispVal] -> IOThrowsError LispVal
printobj [obj] = printobj [obj, Port stdout]
printobj [String s, Port port] = liftIO $ hPutStr port s >> return (Bool True)
printobj [Character c, Port port] = liftIO $ hPutStr port (c:"") >> return (Bool True)
printobj [obj, Port port] = liftIO $ hPutStr port (show obj) >> return (Bool True)
printobj badArgList = throwError $ NumArgs 2 badArgList

-- | display a newline
newline :: [LispVal] -> IOThrowsError LispVal
newline [] = writeProc [Character '\n', Port stdout]
newline [Port port] =  writeProc [Character '\n', Port port]
newline badArgList = throwError $ NumArgs 1 badArgList

-- | Write and append a newline
println :: [LispVal] -> IOThrowsError LispVal
println [] = newline []
println (obj:rest) = printobj [obj] >> println rest


-- | Reads the whole file into a string in memory. Thin wrapper around readFile
readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

-- | Read and parse a file full of Lisp statements and return a list
loadHelper :: String -> IOThrowsError [LispVal]
loadHelper filename = do 
    isexists <- liftIO $ doesFileExist filename 
    if isexists then 
        liftIO (readFile filename) >>= liftThrows . readExprList
    else throwError $ Default $ "Could not load file " ++ filename

-- | Wraps loadHelper returned list into a LispVal List constructor
readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ loadHelper filename

