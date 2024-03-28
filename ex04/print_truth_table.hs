import Data.Bits (Bits (complement, xor, (.&.), (.|.)))

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

data Packet a b = Op a | Value b deriving (Show)

print_truth_table input =
  do
    putStrLn $ showLine vars <> " = |"
    putStrLn $ showSeperator (length vars + 1)
    putStr $ showSquare res
  where
    tree = parseTree input
    vars = evalVariableTable tree
    bol = genBoolTable $ length vars
    resCol = evalTree vars bol tree
    res = mergeCol bol resCol

-- parsing the tree

parseTree = head . go []
  where
    go [t] [] = [t]
    go _ [] = error "stack should only contain one element at the end"
    go stack (c : xc) = go (parseOp c stack) xc

parseOp val x
  | val `elem` ['A' .. 'Z'] = Node (Value val) Empty Empty : x
parseOp op [] = error ("no value to do op \'" ++ [op] ++ "\'")
parseOp '!' (x : xs) = Node (Op '!') x Empty : xs
parseOp op [_] = error ("not enough values to do op \'" ++ [op] ++ "\'")
parseOp op (a : b : xab)
  | op `elem` "&|^>=" = Node (Op op) b a : xab
parseOp c _ = error ("unknown character \'" ++ [c] ++ "\' found")

-- evaluating the tree

evalTree _ _ Empty = error "Cannot evaluate empty node"
evalTree varList boolTable (Node (Value val) _ _) = getValBoolTable val varList boolTable
evalTree varList boolTable (Node (Op op) a b) = zipWith (eval op) (evalTree varList boolTable a) (evalTree varList boolTable b)

genBoolTable = go []
  where
    go l n | n <= 0 = l
    go [] n = go [[False], [True]] (n - 1)
    go l n = go [x <> y | x <- l, y <- [[False], [True]]] (n - 1)

evalVariableTable :: Tree (Packet a Char) -> [Char]
evalVariableTable = removeDuplicates . go []
  where
    removeDuplicates a = foldr (\x acc -> (<>) acc ([x | x `notElem` acc])) [] (reverse a)
    go _ Empty = []
    go x (Node (Value v) _ _) = x <> [v]
    go x (Node (Op _) a b) = x <> go [] a <> go [] b

eval '!' = opposit where opposit a _ = complement a
eval '&' = (.&.)
eval '|' = (.|.)
eval '^' = xor
eval '>' = materialCondition where materialCondition a b = complement a .|. b
eval '=' = (==)

-- test variables

t = parseTree "AB&C|"

v = evalVariableTable t

b = genBoolTable $ length v

-- utility functions

getValBoolTable val hdr = getCol (findVal val hdr)

getCol n = map (!! n)

mergeCol = zipWith (\a b -> a <> [b])

findVal val = go 0
  where
    go acc [] = error ("Value " <> show val <> " not in list")
    go acc (x : xs)
      | x == val = acc
      | otherwise = go (acc + 1) xs

-- printing functions

printTree Empty = ""
printTree (Node (Value v) Empty Empty) = showStrip v
printTree (Node (Op op) l r) = "(" <> printTree l <> showStrip op <> printTree r <> ")"

showStrip s = stripQuotes $ show s

-- stripQuotes = foldr (\ x -> (<>) ([x | x `notElem` "\'\""])) []
stripQuotes s = [x | x <- s, x `notElem` "\'\""]

showSquare :: [[Bool]] -> [Char]
showSquare = foldr (\x -> (<>) (showLine (convertBool x) <> "\n")) []

convertBool x = [(if y then '1' else '0') | y <- x]

showLine arr = "|" <> foldr (\x -> (<>) (" " <> [x] <> " |")) "" arr

showSeperator n = "|" <> concat (replicate n "---|")

putSquare header =
  do
    putStrLn $ showLine header <> " = |"
    putStrLn $ showSeperator (length header + 1)
    putStr $ showSquare $ genBoolTable (length header)
