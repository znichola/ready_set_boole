import Data.Bits (Bits (complement, xor, (.&.), (.|.)))
import Distribution.Compat.CharParsing (oneOf)

data Tree = Nullary Char | Unary Char Tree | Binary Char Tree Tree deriving (Show, Eq)

data Packet a b = Op a | Value b deriving (Show)

sat :: String -> Bool
sat = or . evalRPN

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

-- unit tests

satInput = ["AB|", "AB&", "AA!&", "AA^", "AA!&", "AB^", "AA^", "AA!>", "AB=", "AB&A!B!&&", "ABCDE&&&&", "ABCDE^^^^"]

satOutput = [True, True, False, False, False, True, False, True, True, False, True, True]

checkSat = zipWith (\x y -> sat x == y) satInput satOutput

runTests = do
  putStrLn $ "testing sat for previous examples : " <> pass checkSat
  where
    pass v = if and v then "Pass" else "Fail"
