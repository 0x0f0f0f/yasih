module Main where

import Yasih

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


usage = do
    hPutStrLn stderr ("Usage: \nyasih filename\t\t(run program)\n"
        ++ "yasih -e EXPR\t\t(evaluate an expression)\n"
        ++ "yasih\t\t\t(run the repl)\n"
        ++ "yasih -s filename\t(run program and show evaluation results)")

-- |Parse and eval the first argument
-- Or enter into a REPL Loop
main :: IO ()
main = do
    -- handle include paths
    homedir <- getHomeDirectory
    ipathStr <- lookupEnv "TOY_SCHEME_INCLUDE"
    ipaths <- case ipathStr of 
        Just x -> return $ split x ':'
        Nothing -> return ["/usr/lib/yasih/", homedir ++ "/.local/lib/yasih/"]
    
    -- filter out existing paths
    epaths <- filterM doesDirectoryExist ipaths

    args <- getArgs
    case args of 
        [] -> runRepl epaths -- No argument is passed => run the REPL
        ["-e", expr] -> runOne epaths expr              -- Run a single expression
        ("-s" : rest) -> runProgram epaths rest True -- Run the program and show evaluation result
        ["-h"] -> usage
        ["--help"] -> usage
        [filename] -> runProgram epaths args False      -- Just run the program