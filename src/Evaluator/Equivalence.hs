module Evaluator.Equivalence where 

import LispTypes
import Environment
import Evaluator.Operators

import Control.Monad.Except

equivalencePrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
equivalencePrimitives = 
    [("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal)]

-- Equivalence primitive functions

-- |eqv checks for the equivalence of two items
eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool x, Bool y]           = return $ Bool $ x == y
eqv [Number x, Number y]       = return $ Bool $ x == y
eqv [Float x, Float y]         = return $ Bool $ x == y
eqv [Ratio x, Ratio y]         = return $ Bool $ x == y
eqv [Complex x, Complex y]     = return $ Bool $ x == y
eqv [Character x, Character y] = return $ Bool $ x == y
eqv [String x, String y]       = return $ Bool $ x == y
eqv [Atom x, Atom y]           = return $ Bool $ x == y

-- eqv clause for Dotted list builds a full list and calls itself on it
eqv [DottedList xs x, DottedList ys y]
    = eqv [List $ xs ++ [x], List $ ys ++ [y]] 

-- use the helper function eqvList using eqv to compare pair by pair
eqv [l1@(List x), l2@(List y)] = eqvList eqv [l1, l2]
eqv [_, _] = return $ Bool False 
eqv badArgList = throwError $ NumArgs 2 badArgList

-- |Helper function to check for the equivalence of a pair of items
eqvPair (j, k) = case eqv [j, k] of
    Left err -> False 
    Right (Bool val) -> val

-- |Check equivalence of two items with weak typing 
-- (equal? 2 "2") should return #t while (eqv? 2 "2") => #f
-- This approach uses Existential Types, a ghc extension that
-- allows for heterogenous lists subject to typeclass constraints

equal :: [LispVal] -> ThrowsError LispVal
-- use the helper function eqvList using equal to compare pair by pair
equal [l1@(List x), l2@(List y)] = eqvList equal [l1, l2]
-- eqv clause for Dotted list builds a full list and calls itself on it
equal [DottedList xs x, DottedList ys y]
    = equal [List $ xs ++ [x], List $ ys ++ [y]] 
equal [x, y] = do 
    -- Make an heterogenous list of [unpackNum, unpackStr, unpackBool]\
    -- and then map the partially applied unpackEquals over it, giving a list
    -- of Bools. We use 'or' to return true if any one of them is true.
    primitiveEquals <- liftM or $ mapM (unpackEquals x y) 
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    -- Simply test the two arguments with eqv?, since eqv? is stricter than
    -- equal? return true whenever eqv? does
    eqvEquals <- eqv [x, y]
    -- Return a disjunction of eqvEquals and primitiveEquals
    return $ Bool $ primitiveEquals || let (Bool x) = eqvEquals in x
equal badArgList = throwError $ NumArgs 2 badArgList

-- |Helper function that checks for the equivalence of items in two lists
-- accepts a function as the first argument to allow for both strong/weak equivalence
eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [List x, List y] = return $ Bool $ (length x == length y)
        && all eqvPair (zip x y)


