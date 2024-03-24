{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

import Data.Bits (Bits(complement, (.&.), (.|.), xor))

data Op a = Unary (a -> a) | Binary (a -> a -> a)
data Data a b = Root a | Leaf b deriving (Show)
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)


printTree Empty = ""
printTree (Node value Empty Empty) = [value]
printTree (Node value left right) = "(" ++ printTree right ++ " " ++ [value] ++ " " ++ printTree left ++ ")"

eval_formula = head . go []
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

materialCondition a b = complement a .|. b
