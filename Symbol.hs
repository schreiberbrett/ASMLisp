module Symbol (Symbol (Symbols, Atom), toSymbols, isAtom) where

import Data.Char (isSpace, isAlphaNum)
import Data.List (groupBy)

-- Lexical Analysis
data Symbol = Atom String | Symbols [Symbol] deriving Show

toSymbols :: String -> [Symbol]
toSymbols = parse . tokenize

tokenize :: String -> [String]
tokenize = filter (all (not . isSpace)) . groupBy (both isAlphaNum) where
        both predicate x y = predicate x && predicate y

parse :: [String] -> [Symbol]
parse ("(":xs) = Symbols (parse left) : parse right where
        (left, right) = splitOnClosingParen xs
parse (")":xs) = parse xs
parse (x:xs) = Atom x : parse xs
parse _ = []

splitOnClosingParen :: [String] -> ([String], [String])
splitOnClosingParen strings = go strings 1 where
        go xs 0 = ([], xs)
        go (x:xs) n = (x:left, right) where
                (left, right) = go xs $ case x of
                        "(" -> n + 1
                        ")" -> n - 1
                        _ -> n
        go [] n = error $ "Got to depth: " ++ show n

isAtom :: Symbol -> Bool
isAtom (Atom _) = True
isAtom _ = False

