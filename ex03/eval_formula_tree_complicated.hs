{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Data.Bits (Bits(complement, (.&.), (.|.), xor))

data Op a = Unary (a -> a) | Binary (a -> a -> a)
data Data a b = Root a | Leaf b deriving (Show)
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

printTree Empty = ""
printTree (Node value Empty Empty) = show value
printTree (Node value left right) = "(" ++ printTree right ++ " " ++ show value ++ " " ++ printTree left ++ ")"

eval_formula = go []
  where
    go stack [] = stack
    go stack (c : xc) = go (parsOp c stack) xc

parsOp '1' x             = Node (Leaf False) Empty Empty : x
parsOp '0' x             = Node (Leaf False) Empty Empty : x
parsOp _ []              = error "no value to do op with"
parsOp '!' (a : xa)      = Node (Root (Unary complement)) a Empty : xa
parsOp _ [_]             = error "not enough values to do op"
parsOp '&' (a : b : xab) = Node (Root (Binary (.&.))) a b : xab
parsOp '|' (a : b : xab) = Node (Root (Binary (.|.))) a b : xab
parsOp '^' (a : b : xab) = Node (Root (Binary xor)) a b : xab
parsOp '>' (a : b : xab) = Node (Root (Binary materialCondition)) a b : xab
parsOp '=' (a : b : xab) = Node (Root (Binary (==))) a b: xab
parsOp c _               = error ("unknown charaster \'" ++ [c] ++ "\' found")

materialCondition a b = complement a .|. b
