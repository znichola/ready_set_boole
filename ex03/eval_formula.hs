module Main where

import Data.Bits (Bits (complement, xor, (.&.), (.|.)))

data Tree a = Empty | Node a (Tree a) (Tree a)

data Packet a b = Branch a | Leaf b

data Op = Not | And | Or | Xor | Imply | Equal

eval_formula = evalTree . checkParsing . parseTree
  where
    checkParsing [x] = x
    checkParsing _ = error "stack should only contain one element"

evalTree Empty = error "cannot evaluate empty node"
evalTree (Node (Leaf value) _ _) = value
evalTree (Node (Branch op) a b) = eval op (evalTree a) (evalTree b)

eval Not = opposit where opposit a _ = complement a
eval And = (.&.)
eval Or = (.|.)
eval Xor = xor
eval Imply = materialCondition where materialCondition a b = complement a .|. b
eval Equal = (==)

parseTree = go []
  where
    go stack [] = stack
    go stack (c : xc) = go (parsOp c stack) xc

parsOp '1' x             = Node (Leaf True) Empty Empty : x
parsOp '0' x             = Node (Leaf False) Empty Empty : x
parsOp _ []              = error "no value to do op with"
parsOp '!' (a : xa)      = Node (Branch Not) a Empty : xa
parsOp _ [_]             = error "not enough values to do op"
parsOp '&' (a : b : xab) = Node (Branch And) a b : xab
parsOp '|' (a : b : xab) = Node (Branch Or) a b : xab
parsOp '^' (a : b : xab) = Node (Branch Xor) a b : xab
parsOp '>' (a : b : xab) = Node (Branch Imply) a b : xab
parsOp '=' (a : b : xab) = Node (Branch Equal) a b : xab
parsOp c _               = error ("unknown charaster \'" ++ [c] ++ "\' found")

main = do
  let tr = parseTree "1011|&="
  print tr
  print $ eval_formula "1011|&="

-- helpter function for printing the tree

instance (Show a) => Show (Tree a) where
  show Empty = ""
  show (Node value Empty Empty) = stripQuotes $ show value
  show (Node value left right) = "(" ++ show right ++ " " ++ stripQuotes (show value) ++ " " ++ show left ++ ")"

instance Show Op where
  show Not = "!"
  show And = "&"
  show Or = "|"
  show Xor = "^"
  show Imply = ">"
  show Equal = "="

instance (Show a, Show b) => Show (Packet a b) where
  show (Leaf value) = show value
  show (Branch value) = show value

stripQuotes [] = []
stripQuotes "\"" = []
stripQuotes "\'" = []
stripQuotes ('\"' : xs) = stripQuotes xs
stripQuotes ('\'' : xs) = stripQuotes xs
stripQuotes (x : xs) = x : stripQuotes xs
