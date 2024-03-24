{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Main where

import Data.Bits ( Bits((.|.), (.&.), xor, complement) )

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show Empty = ""
  show (Node value Empty Empty) = stripQuotes $ show value
  show (Node value left right) = "(" ++ show right ++ " " ++ stripQuotes (show value) ++ " " ++ show left ++ ")"

data Op = Not | And | Or | Xor | Imply | Equal

instance Show Op where
  show Not = "!"
  show And = "&"
  show Or = "|"
  show Xor = "^"
  show Imply = ">"
  show Equal = "="

eval_formula s = f $ evalTree $ head $ parseTree s

evalTree Empty = error "wtf"
evalTree (Node value Empty Empty) = value
evalTree (Node '!' l _) = t $ complement (f $ evalTree l)
evalTree (Node '&' l r) = t $ (.&.) (f $ evalTree l) (f $ evalTree r)
evalTree (Node '|' l r) = t $ (.|.) (f $ evalTree l) (f $ evalTree r)
evalTree (Node '^' l r) = t $ xor (f $ evalTree l) (f $ evalTree r)
evalTree (Node '>' l r) = t $ (.|.) (complement $ f $ evalTree l) (f $ evalTree r)
evalTree (Node '=' l r) = t $ (==) (f $ evalTree l) (f $ evalTree r)


parseTree = go []
  where
    go stack [] = stack
    go stack (c : xc) = go (parsOp c stack) xc

parsOp '1' x             = Node '1' Empty Empty : x
parsOp '0' x             = Node '0' Empty Empty : x
parsOp _ []              = error "no value to do op with"
parsOp '!' (a : xa)      = Node '!' a Empty : xa
parsOp _ [_]             = error "not enough values to do op"
parsOp '&' (a : b : xab) = Node '&' a b : xab
parsOp '|' (a : b : xab) = Node '|' a b : xab
parsOp '^' (a : b : xab) = Node '^' a b : xab
parsOp '>' (a : b : xab) = Node '>' a b : xab
parsOp '=' (a : b : xab) = Node '=' a b : xab
parsOp c _               = error ("unknown charaster \'" ++ [c] ++ "\' found")

main = do
  let tr = parseTree "1011||="
  print tr
  print $ eval_formula "1011||="

t b = if b then '1' else '0'
f '1' = True
f '0' = False

stripQuotes [] = []
stripQuotes "\"" = []
stripQuotes "\'" = []
stripQuotes ('\"':xs) = stripQuotes xs
stripQuotes ('\'':xs) = stripQuotes xs
stripQuotes (x:xs) = x : stripQuotes xs
