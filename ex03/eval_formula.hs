module Main where

import Data.Bits (Bits (complement, xor, (.&.), (.|.)))

data Tree a = Empty | Node a (Tree a) (Tree a)

data Packet a b = Branch a | Leaf b

data Op = Not | And | Or | Xor | Imply | Equal

eval_formula = evalTree . parsTree

evalTree Empty = error "cannot evaluate empty node"
evalTree (Node (Leaf value) _ _) = value
evalTree (Node (Branch op) a b) = eval op (evalTree a) (evalTree b)

eval Not = opposit where opposit a _ = complement a
eval And = (.&.)
eval Or = (.|.)
eval Xor = xor
eval Imply = materialCondition where materialCondition a b = complement a .|. b
eval Equal = (==)

parsTree = head . go []
  where
    go [t] [] = [t]
    go _ [] = error "stack should only contain one element at the end"
    go stack (c : xc) = go (parsOp c stack) xc

parsOp '1' x             = Node (Leaf True) Empty Empty : x
parsOp '0' x             = Node (Leaf False) Empty Empty : x
parsOp op []             = error ("no value to do op \'" ++ [op] ++ "\'")
parsOp '!' (a : xa)      = Node (Branch Not) a Empty : xa
parsOp op [_]            = error ("not enough values to do op \'" ++ [op] ++ "\'")
parsOp '&' (a : b : xab) = Node (Branch And) a b : xab
parsOp '|' (a : b : xab) = Node (Branch Or) a b : xab
parsOp '^' (a : b : xab) = Node (Branch Xor) a b : xab
parsOp '>' (a : b : xab) = Node (Branch Imply) a b : xab
parsOp '=' (a : b : xab) = Node (Branch Equal) a b : xab
parsOp c _               = error ("unknown charaster \'" ++ [c] ++ "\' found")

main = do
  let expr = "1011||="
  putStrLn $ printTree $ parsTree expr
  print $ eval_formula expr

-- helpter functions for printing the tree

printTree Empty = ""
printTree (Node (Leaf v) _ _) = if v then "1" else "0"
printTree (Node (Branch op) l r) = "(" <> printTree l <> show op <> printTree r <>")"

instance (Show a) => Show (Tree a) where
  show Empty = ""
  show (Node value Empty Empty) = stripQuotes $ show value
  show (Node value left right) = "(" <> show right <> " " <> stripQuotes (show value) <> " " <> show left <> ")"

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

stripQuotes = foldr (\ x -> (<>) ([x | x `notElem` "\'\""])) []
-- below is what i wrote, but after accepting a series of
-- hlint proposals I got the above
-- stripQuotes [] = []
-- stripQuotes (x : xs) = (if x `elem` "\'\"" then [] else [x] ) <> stripQuotes xs
