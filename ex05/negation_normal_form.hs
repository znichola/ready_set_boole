import Data.Bits (Bits (complement, xor, (.&.), (.|.)))

data Tree a = Nullary a | Unary a (Tree a) | Binary a (Tree a) (Tree a) deriving (Show, Eq)

negation_normal_form = putStrLn . showTreeRPN . rewriteTree . parseTree

doNothing = putStrLn . showTreeRPN . parseTree

print_truth_table input =
  do
    putStrLn $ showLine vars <> " = |"
    putStrLn $ showSeperator (length vars + 1)
    putStr $ showSquare res
  where
    tree = parseTree input
    vars = evalVars tree
    bol = genBoolTable $ length vars
    resCol = evalTree vars bol tree
    res = mergeCol bol resCol

-- testing values

t = parseTree "AB>"

v = evalVars t

b = genBoolTable $ length v

-- rewriting the tree

rewriteTree (Nullary term) = Nullary term
rewriteTree (Unary op a) =
  case (op, a) of
    -- Elimination of double negation
    ('!', Unary '!' a) -> rewriteTree a
    -- De Morgan’s laws: or
    ('!', Binary '|' a b) -> rewriteTree (Binary '&' (Unary '!' a) (Unary '!' b))
    -- De Morgan’s laws: and
    ('!', Binary '&' a b) -> rewriteTree (Binary '|' (Unary '!' a) (Unary '!' b))
    _ -> Unary op (rewriteTree a)
rewriteTree (Binary op a b) =
  case (op, a, b) of
    -- Material conditions
    ('>', a, b) -> rewriteTree (Binary '|' (Unary '!' a) b)
    -- Equivalence
    -- ('=', a, b) -> rewriteTree (Binary '&' (Binary '>' a b) (Binary '>' b a))
    -- Equivalence' to match example and remove unwanted > operations (A&B)|((!A)&(!B))
    ('=', a, b) -> rewriteTree (Binary '|' (Binary '&' a b) (Binary '&' (Unary '!' a) (Unary '!' b)))
    -- Distributivity: and
    -- ('&', a, Binary '|' b c) -> rewriteTree (Binary '|' (Binary '&' a b) (Binary '&' a c))
    -- Distributivity: or
    -- ('|', a, Binary '&' b c) -> rewriteTree (Binary '&' (Binary '|' a b) (Binary '|' a c))
    -- Distributivity: and'
    -- ('|', Binary '&' a b, Binary '&' a' c) | a == a' -> rewriteTree (Binary '&' a (Binary '|' b c))
    -- Distributivity: or'
    -- ('&', Binary '|' a b, Binary '|' a' c) | a == a' -> rewriteTree (Binary '|' a (Binary '&' b c))
    -- enabeling both of these causes an infinate loop where more and more ops get added
    _ -> Binary op (rewriteTree a) (rewriteTree b)

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
parseTerm term (x : y : stack) | term `elem` "&|^>=" = Binary term y x : stack
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

showSquare = foldr (\x -> (<>) (showLine (convertBool x) <> "\n")) []

convertBool x = [(if y then '1' else '0') | y <- x]

showLine arr = "|" <> foldr (\x -> (<>) (" " <> [x] <> " |")) "" arr

showSeperator n = "|" <> concat (replicate n "---|")

putSquare header =
  do
    putStrLn $ showLine header <> " = |"
    putStrLn $ showSeperator (length header + 1)
    putStr $ showSquare $ genBoolTable (length header)

-- unit testing

nnfInput = ["AB&!", "AB|!", "AB>", "AB=", "AB|C&!"]

nnfOutput = ["A!B!|", "A!B!&", "A!B|", "AB&A!B!&|", "A!B!&C!|"]

ttInput = ["AB&C|", "AB&", "AB|", "AB|C&", "ABC|&"]

ttOutput = ["(A&B)|C", "A&B", "A|B", "(A|B)&C", "A&(B|C)"]

creazyTrees = ["A!!B!CD|^!!&A>E!=", "AB>A>E>C>D="]

allRPNs = nnfInput <> nnfOutput <> ttInput <> creazyTrees

checkShowRPN = map (\x -> x == showTreeRPN (parseTree x)) allRPNs

checkNNFtree = zipWith (\x y -> rewriteTree (parseTree x) == parseTree y) nnfInput nnfOutput

checkNNFbools = zipWith (\x y -> evalTreeSimple (rewriteTree $ parseTree x) == evalTreeSimple (parseTree y)) nnfInput nnfOutput

checkNNFstring = zipWith (\x y -> showTreeRPN (rewriteTree $ parseTree x) == y) nnfInput nnfOutput

runTests = do
  putStrLn $ "testing RPN print from parsed AST   : " <> pass checkShowRPN
  putStrLn $ "testing binary result after rewrite : " <> pass checkNNFtree
  putStrLn $ "testing bools result after rewrite  : " <> pass checkNNFbools
  putStrLn $ "testing string result after rewrite : " <> pass checkNNFstring
  where
    pass v = if and v then "Pass" else "Fail"
