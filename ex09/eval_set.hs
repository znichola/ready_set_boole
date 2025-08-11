import Data.ByteString (group)
import Data.Function ((&))
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Set (Set, difference, intersection, union)
import Data.Set qualified as Set
import Data.Tuple (fst, snd)
import Debug.Trace (trace)

-- TODO : REWRITE to not use Sets as they are a math funciton!

data Tree = Nullary Char | Unary Char Tree | Binary Char Tree Tree deriving (Show, Eq)

type Rule = Tree -> Maybe Tree

eval_set :: [Char] -> [[Int]] -> [Int]
eval_set expr sets =
  let vars = getVars expr
      tree = rewriteTree $ parseTree expr
      setMap = Map.fromList $ zip ('u' : getVars expr) (map Set.fromList (concat sets : sets))
   in Set.toList $ evalTree setMap tree

-- evaluating the tree

evalTree :: Map Char (Set Int) -> Tree -> Set Int
evalTree setMap (Nullary var) =
  case setMap !? var of
    Just res -> res
    Nothing -> error $ "Value not found in set " <> [var]
evalTree setMap (Unary op a) = eval op (evalTree setMap (Nullary 'u')) (evalTree setMap a)
evalTree setMap (Binary op a b) = eval op (evalTree setMap a) (evalTree setMap b)

eval '|' = union
eval '&' = intersection
eval '!' = difference

-- utility function

getVars :: [Char] -> [Char]
getVars = unique . filter (\x -> x `elem` ['A' .. 'Z'])
  where
    unique a = foldr (\x acc -> (<>) acc ([x | x `notElem` acc])) [] (reverse a)

setMap :: [Char] -> [[Int]] -> Map Char (Set Int)
setMap exp sets = Map.fromList $ zip ('u' : getVars exp) (map Set.fromList (concat sets : sets))

-- rewrite the tree, to remove ops that are annoying to do on sets,
-- eg implies as this needs to know a third variable, the universe of possible numbers

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

rewriteTree = rebalanceTree . rewriteTreeTopDown ruleSet

rewriteTreeTopDown :: [Rule] -> Tree -> Tree
rewriteTreeTopDown rules tree =
  let rewrite = rewriteTreeTopDown rules -- defined locally
   in case applyRules rules tree of
        Just tree' -> rewrite tree' -- tree changes on this "level", re-run it through the rules
        Nothing ->
          -- tree does not change, apply rules to children
          case tree of
            Nullary t -> Nullary t
            Unary op t -> Unary op (rewrite t)
            Binary op l r -> Binary op (rewrite l) (rewrite r)

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
   in head reBalanced
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

-- Simplify final expression to remove unwanted '>' ops, res: (A&B)|((!A)&(!B))
equivilenceSimplified (Binary '=' a b) = Just $ Binary '|' (Binary '&' a b) (Binary '&' (Unary '!' a) (Unary '!' b))
equivilenceSimplified _ = Nothing

equivilence (Binary '=' a b) = Just $ Binary '&' (Binary '>' a b) (Binary '>' a b)
equivilence _ = Nothing

removeXor (Binary '^' a b) = Just $ Binary '&' (Binary '|' a b) (Binary '|' (Unary '!' a) (Unary '!' b))
removeXor _ = Nothing

distributivityAnd (Binary '&' a (Binary '|' b c)) = Just $ Binary '|' (Binary '&' a b) (Binary '&' a c)
distributivityAnd _ = Nothing

-- the inverse is usefull for simplifying the expression!
distributivityAnd' (Binary '|' (Binary '&' a b) (Binary '&' a' c)) | a == a' = Just $ Binary '&' a (Binary '|' b c)
distributivityAnd' _ = Nothing

distributivityOr (Binary '|' a (Binary '&' b c)) = Just $ Binary '&' (Binary '|' a b) (Binary '|' a c)
distributivityOr _ = Nothing

distributivityOr' (Binary '&' (Binary '|' a b) (Binary '|' a' c)) | a == a' = Just $ Binary '|' a (Binary '&' b c)
distributivityOr' _ = Nothing

-- parsing the tree

parseTree = head . go []
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

showTree tree = (\x -> if length x >= 3 then init $ tail x else x) $ go tree
  where
    go (Nullary v) = showStrip v
    go (Unary op left) = "(" <> showStrip op <> go left <> ")"
    go (Binary op left right) = "(" <> go left <> showStrip op <> go right <> ")"

showStrip s = [x | x <- show s, x `notElem` "\'\""]

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
    [[0],[0],[0]],
    [[0],[0],[0]]
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
        (eval_set x y == z)
          || trace
            ("\nFOR \"" ++ x ++ "\"\nEXPECTED\n" ++ show z ++ "\nACTUAL\n" ++ show (eval_set x y))
            False
    )
    formulaInput
    setInput
    evalOutput

runTests = do
  putStrLn $ "testing sat for previous examples : " <> pass checkEval
  where
    pass v = if and v then "Pass" else "Fail"
