import Data.List (group, intersect, sort, uncons)
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Tuple (fst, snd)
import Debug.Trace (trace)

data Tree = Nullary Char | Unary Char Tree | Binary Char Tree Tree deriving (Show, Eq)

type Rule = Tree -> Maybe Tree

eval_set :: [Char] -> [[Int]] -> [Int]
eval_set expr sets =
  let vars = getVars expr
      tree = rewriteTree $ parseTree expr
      setMap = Map.fromList $ zip ('u' : getVars expr) (map unique (concat sets : sets))
   in evalTree setMap tree

-- evaluating the tree

evalTree :: Map Char [Int] -> Tree -> [Int]
evalTree setMap (Nullary var) =
  case setMap !? var of
    Just res -> res
    Nothing -> error $ "Value not found in set " <> [var]
evalTree setMap (Unary op a) = eval op (evalTree setMap (Nullary 'u')) (evalTree setMap a)
evalTree setMap (Binary op a b) = eval op (evalTree setMap a) (evalTree setMap b)

eval '|' a b = unique $ a <> b
eval '&' a b = intersection a b
eval '!' a b = difference a b

-- utility function

getVars = unique . filter (\x -> x `elem` ['A' .. 'Z'])

unique l = map head' (group $ sort l)



intersection [] _ = []
intersection (x : xs) ys
  | x `elem` ys = x : intersection xs ys
  | otherwise = intersection xs ys

difference [] _ = []
difference (x : xs) ys
  | x `notElem` ys = x : difference xs ys
  | otherwise = difference xs ys

ruleSet =
  [ eliminationOfDoubleNegative,
    deMorgansLawAnd,
    deMorgansLawOr,
    materialCondition,
    equivilenceSimplified,
    removeXor,
    distributivityAnd',
    distributivityOr
  ]

rewriteTree = rebalanceTree . rewriteTreeBottomUp ruleSet

rewriteTreeBottomUp :: [Rule] -> Tree -> Tree
rewriteTreeBottomUp _ (Nullary op) = Nullary op
rewriteTreeBottomUp rules (Unary op t) =
  let t' = rewriteTreeBottomUp rules t
   in case applyRules rules (Unary op t') of
        Just res -> rewriteTreeBottomUp rules res
        Nothing -> Unary op t'
rewriteTreeBottomUp rules (Binary op l r) =
  let l' = rewriteTreeBottomUp rules l
      r' = rewriteTreeBottomUp rules r
   in case applyRules rules (Binary op l' r') of
        Just res -> rewriteTreeBottomUp rules res
        Nothing -> Binary op l' r'

applyRules :: [Rule] -> Tree -> Maybe Tree
applyRules [] tree = Nothing
applyRules (rule : xs) tree =
  case rule tree of
    Just t' -> Just t'
    Nothing -> applyRules xs tree

-- rewrite rules

rebalanceTree = leftLeanOp '|' . leftLeanOp '&'

leftLeanOp op tree =
  let flattened = splat [] tree
      reBalanced = shiftLeft $ reverse flattened
   in head' reBalanced
  where
    splat xs (Binary op' a b)
      | op' == op =
          splat [] a <> splat [] b <> xs
    splat xs tree = tree : xs -- here is where you would think this needs to be recursive,
    -- but no, it's fine, all other symbols are factored out at this point
    shiftLeft [] = error "Cannot left lean an empty list"
    shiftLeft [one] = [one]
    shiftLeft (x : y : ss) = shiftLeft $ Binary op y x : ss

eliminationOfDoubleNegative (Unary '!' (Unary '!' a)) = Just a
eliminationOfDoubleNegative _ = Nothing

deMorgansLawAnd (Unary '!' (Binary '&' a b)) = Just $ Binary '|' (Unary '!' a) (Unary '!' b)
deMorgansLawAnd _ = Nothing

deMorgansLawOr (Unary '!' (Binary '|' a b)) = Just $ Binary '&' (Unary '!' a) (Unary '!' b)
deMorgansLawOr _ = Nothing

materialCondition (Binary '>' a b) = Just $ Binary '|' (Unary '!' a) b
materialCondition _ = Nothing

equivilenceSimplified (Binary '=' a b) = Just $ Binary '|' (Binary '&' a b) (Binary '&' (Unary '!' a) (Unary '!' b))
equivilenceSimplified _ = Nothing

removeXor (Binary '^' a b) = Just $ Binary '&' (Binary '|' a b) (Binary '|' (Unary '!' a) (Unary '!' b))
removeXor _ = Nothing

-- the inverse is usefull for simplifying the expression!
distributivityAnd' (Binary '|' (Binary '&' a b) (Binary '&' a' c)) | a == a' = Just $ Binary '&' a (Binary '|' b c)
distributivityAnd' _ = Nothing

distributivityOr (Binary '|' a (Binary '&' b c)) = Just $ Binary '&' (Binary '|' a b) (Binary '|' a c)
distributivityOr _ = Nothing

-- parsing the tree

parseTree = head' . go []
  where
    go [t] [] = [t]
    go _ [] = error "stack should only contain one element at the end"
    go stack (t : xt) = go (parseTerm t stack) xt

parseTerm term stack | term `elem` ['A' .. 'Z'] = Nullary term : stack
parseTerm term []
  | term `elem` "!&|^>=" = error ("No value to do op \'" ++ [term] ++ "\'")
parseTerm '!' (x : stack) = Unary '!' x : stack
parseTerm term [_]
  | term `elem` "&|^>=" = error ("Not enough values to do op \'" ++ [term] ++ "\'")
parseTerm term (x : y : stack) | term `elem` "&|^>=" = Binary term y x : stack -- reverse order to postfix, aka RPN notation
parseTerm c _ = error ("unknown character \'" ++ [c] ++ "\' found")

-- printing function

showTreeRPN = go
  where
    go (Nullary term) = [term]
    go (Unary op left) = go left <> [op]
    go (Binary op left right) = go left <> go right <> [op]

showTree tree = (\x -> if length x >= 3 then init $ tail' x else x) $ go tree
  where
    go (Nullary v) = showStrip v
    go (Unary op left) = "(" <> showStrip op <> go left <> ")"
    go (Binary op left right) = "(" <> go left <> showStrip op <> go right <> ")"

showStrip s = [x | x <- show s, x `notElem` "\'\""]

-- util to remove warnings

head' [] = error "Can't get head on empty list"
head' (x:_) = x
-- unique a = foldr (\x acc -> (<>) acc ([x | x `notElem` acc])) [] (reverse a)

tail' = drop 1

-- unit testing

formulaInput =
  [ "AB&",
    "AB|",
    "ABC||",
    "ABC||",
    "ABC&&",
    "A!",
    "A!B&",
    "A!BC|>B=",
    "ABC^^",
    "ABC>>"
  ]

setInput =
  [ [[0, 1, 2], [0, 3, 4]],
    [[0, 1, 2], [3, 4, 5]],
    [[], [], []],
    [[1], [2], [3]],
    [[0], [0], []],
    [[0, 1, 2]],
    [[1, 2, 3], [2, 3, 4]],
    [[0, 1, 2, 4, 5], [-1, 0, 2], [4, 9]],
    [[0], [0], [0]],
    [[0], [0], [0]]
  ]

evalOutput =
  [ [0],
    [0, 1, 2, 3, 4, 5],
    [],
    [1, 2, 3],
    [],
    [],
    [4],
    [-1, 0, 2], -- i trust in my answer
    [0],
    [0]
  ]

checkEval =
  zipWith3
    ( \x y z ->
        eval_set x y == z
          || trace
            ("\nFOR \"" ++ x ++ "\"\nEXPECTED\n" ++ show z ++ "\nACTUAL\n" ++ show (eval_set x y))
            False
    )
    formulaInput
    setInput
    evalOutput

runTests = putStrLn $ "testing sat for previous examples : " <> pass checkEval
  where
    pass v = if and v then "Pass" else "Fail"
