{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use uncurry" #-}
import Control.Monad (when)
import Data.Bits (Bits (complement, xor, (.&.), (.|.)))
import Data.ByteString (group)
import Data.Function ((&))
import Data.Tuple (fst, snd)
import Debug.Trace (trace)

data Tree = Nullary Char | Unary Char Tree | Binary Char Tree Tree deriving (Show, Eq)

type Rule = Tree -> Maybe Tree

eval_set :: [Char] -> [[Int]] -> [Int]
eval_set _ _ = [0]

-- parsing the tree

parseTree = head . go []
  where
    go [t] [] = [t]
    go _ [] = error "stack should only contain one element at the end"
    go stack (t : xt) = go (parseTerm t stack) xt

parseTerm term stack | term `elem` ['A' .. 'Z'] = Nullary term : stack
parseTerm term [] = error ("no value to do op \'" ++ [term] ++ "\'")
parseTerm '!' (x : stack) = Unary '!' x : stack
parseTerm term [_] = error ("not enough values to do op \'" ++ [term] ++ "\'")
parseTerm term (x : y : stack) | term `elem` "&|^>=" = Binary term y x : stack -- reverse order to postfix, aka RPN notation
parseTerm c _ = error ("unknown character \'" ++ [c] ++ "\' found")

-- evaluating the tree

evalRPN = evalTreeSimple . parseTree

evalTreeSimple input = evalTree vars (genBoolTable $ length vars) input
  where
    vars = evalVars input

evalTree vars bools (Nullary term) = getValBoolTable term vars bools
evalTree vars bools (Unary '!' left) = map complement (evalTree vars bools left)
evalTree vars bools (Binary op left right) = zipWith (eval op) (evalTree vars bools left) (evalTree vars bools right)

evalVars = removeDuplicates . go []
  where
    removeDuplicates a = foldr (\x acc -> (<>) acc ([x | x `notElem` acc])) [] (reverse a)
    go xs (Nullary term) = xs <> [term]
    go xs (Unary _ left) = xs <> go [] left
    go xs (Binary _ left right) = xs <> go [] left <> go [] right

genBoolTable num = go [[False], [True]] (num - 1)
  where
    go l n | n <= 0 = l
    go l n = go [x <> y | x <- l, y <- [[False], [True]]] (n - 1)

eval '&' = (.&.)
eval '|' = (.|.)
eval '^' = xor
eval '>' = materialCondition where materialCondition a b = complement a .|. b
eval '=' = (==)

-- utility function

findVal val = go 0
  where
    go acc [] = error ("Value " <> show val <> " not in list")
    go acc (x : xs) | x == val = acc | otherwise = go (acc + 1) xs

getValBoolTable val values = getCol (findVal val values)

getCol n = map (!! n)

mergeCol = zipWith (\a b -> a <> [b])

-- printing function

showTreeRPN = go
  where
    go (Nullary term) = [term]
    go (Unary op left) = go left <> [op]
    go (Binary op left right) = go left <> go right <> [op]

showTree tree = (\x -> if length x >= 3 then init $ tail x else x) $ go tree
  where
    go (Nullary v) = showStrip v
    go (Unary op left) = "(" <> showStrip op <> go left <> ")"
    go (Binary op left right) = "(" <> go left <> showStrip op <> go right <> ")"

showStrip s = [x | x <- show s, x `notElem` "\'\""]

-- unit testing

evalInput =
  [ ( "AB&",
      [ [0, 1, 2],
        [0, 3, 4]
      ]
    ),
    ( "AB|",
      [ [0, 1, 2],
        [3, 4, 5]
      ]
    ),
    ( "A!",
      [[0, 1, 2]]
    )
  ]

evalOutput =
  [ [0],
    [0, 1, 2, 3, 4, 5],
    []
  ]

checkEval =
  zipWith
    ( \x y ->
        (eval_set (fst x) (snd x) == y)
          || trace
            ("\nEXPECTED\n" ++ show y ++ "\nACTUAL\n" ++ show (eval_set (fst x) (snd x)))
            False
    )
    evalInput
    evalOutput

runTests = do
  putStrLn $ "testing sat for previous examples : " <> pass checkEval
  where
    pass v = if and v then "Pass" else "Fail"
