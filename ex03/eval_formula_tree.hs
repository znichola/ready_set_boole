{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Eval_formula where

import Data.Bits ( Bits((.|.), (.&.), xor, complement) )

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

printTree Empty = ""
printTree (Node value Empty Empty) = [value]
printTree (Node value left right) = "(" ++ printTree right ++ " " ++ [value] ++ " " ++ printTree left ++ ")"

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

t b = if b then '1' else '0'
f '1' = True
f '0' = False
