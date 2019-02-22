-- | A straightforward specification of Lisp-like S-expressions and functions
-- for parsing them.
module Symbol
    ( Symbol (..)
    , toSymbols
    , isAtom
    ) where

import Data.Char (isSpace, isAlphaNum)
import Data.List (groupBy)

-- | A data structure corresponding to Lisp's S-expressions.
data Symbol
    -- | An atomic of an S-expression; not a list.
    = Atom String
    -- | A list of symbols
    | Symbols [Symbol]
    deriving Show

-- | Parse a string into a list of 'Symbol's
toSymbols
    :: String    -- ^ A line-delimited String representing a list of S-expressions
    -> [Symbol]  -- ^ The resulting 'Symbol's 
toSymbols = parse . tokenize

tokenize :: String -> [String]
tokenize = filter (all (not . isSpace)) . groupBy (both isAlphaNum)
  where
    both predicate x y = predicate x && predicate y

parse :: [String] -> [Symbol]
parse ("(":xs) = Symbols (parse left) : parse right
  where
    (left, right) = splitOnClosingParen xs
parse (")":xs) = parse xs
parse (x:xs) = Atom x : parse xs
parse _ = []

splitOnClosingParen :: [String] -> ([String], [String])
splitOnClosingParen strings = go strings 1 where
    go xs 0 = ([], xs)
    go [] n = error $ "Got to depth: " ++ show n
    go (x:xs) n = (x:left, right)
      where
        (left, right) = go xs $ case x of
            "(" -> n + 1
            ")" -> n - 1
            _ -> n

-- | Returns 'True' if the given 'Symbol' is an 'Atom', and 'False' otherwise.
isAtom :: Symbol -> Bool
isAtom (Atom _) = True
isAtom _ = False
