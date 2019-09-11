module Main where

import LispRepl

import Control.Monad
import System.IO
import System.Environment
import System.Directory
import Numeric

-- | Helper function to split a string at a delimiter
split :: String -> Char -> [String]
split "" _ = [""]
split (c:cs) d
    | c == d = "" : split cs d 
    | otherwise = (c : head (split cs d)) : tail (split cs d)


-- |Parse and eval the first argument
-- Or enter into a REPL Loop
main :: IO ()
main = do
    -- handle include paths
    homedir <- getHomeDirectory
    ipathStr <- lookupEnv "TOY_SCHEME_INCLUDE"
    ipaths <- case ipathStr of 
        Just x -> return $ split x ':'
        Nothing -> return ["/usr/lib/toy-scheme/", homedir ++ "/.local/lib/toy-scheme/"]
    
    -- filter out existing paths
    epaths <- filterM doesDirectoryExist ipaths 

    args <- getArgs 
    case args of 
        [] -> runRepl epaths -- No argument is passed => run the REPL
        [filename] -> runProgram epaths args
        ["-e", expr] -> runOneExpr epaths expr
        _ -> do
            hPutStrLn stderr "Usage: haskell-toy-scheme [EXPR]"
            hPutStrLn stderr "If EXPR is provided evaluate it. Otherwise run the REPL."