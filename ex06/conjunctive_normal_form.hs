import Data.Bits (Bits (complement, xor, (.&.), (.|.)))
import Control.Monad (when)
import Data.Function ((&))
import Data.ByteString (group)
import Debug.Trace

data Tree = Nullary Char | Unary Char Tree | Binary Char Tree Tree deriving (Show, Eq)

type Rule = Tree -> Maybe Tree

conjunctive_normal_form = putStrLn . showTreeRPN . rewriteTree . parseTree

-- rewriting the tree

ruleSet =
  [ eliminationOfDoubleNegative
    ,deMorgansLawAnd
    ,deMorgansLawOr
    ,materialCondition
    ,equivilenceSimplified
    ,removeXor
    ,distributivityAnd'
    ,distributivityOr
    ]

rewriteTree = rebalanceTree . rewriteTreeTopDown ruleSet

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

leftLeanOp op tree = let
  flattened = splat [] tree
  reBalanced = shiftLeft $ reverse flattened
  in
    head reBalanced
    where
    splat xs (Binary op' a b) |
      op' == op = splat [] a <> splat [] b <> xs
    splat xs tree = tree : xs -- here is where you would think this needs to be recursive,
                              -- but no, it's fine, all other symbols are factored out at this point
    shiftLeft [] = error "Cannot left lean an empty list"
    shiftLeft [one] = [one]
    shiftLeft (x:y:ss) = shiftLeft $ Binary op y x : ss

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

cnfInput = ["AB&!", "AB|!", "AB|C&", "AB|C|D|", "AB&C&D&", "AB&!C!|", "AB|!C!&", "ABCD&|&"]

realOutput = ["A!B!|", "A!B!&", "AB|C&", "AB|C|D|", "AB&C&D&", "A!B!|C!|", "A!B!&C!&", "ABC|BD|&&"]

cnfOutput = ["A!B!|", "A!B!&", "AB|C&", "ABCD|||", "ABCD&&&", "A!B!C!||", "A!B!C!&&"]

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

checkCNFtree = zipWith (\x y -> rewriteTree (parseTree x) == parseTree y) cnfInput cnfOutput

checkCNFbools = zipWith (\x y -> evalTreeSimple (rewriteTree $ parseTree x) == evalTreeSimple (parseTree y)) cnfInput cnfOutput

checkCNFstring = zipWith (\x y -> showTreeRPN (rewriteTree $ parseTree x) == y) cnfInput cnfOutput

runTests = do
  putStrLn $ "testing RPN print from parsed AST       : " <> pass checkShowRPN
  putStrLn $ "NNF testing bools result after rewrite  : " <> pass checkNNFbools
  putStrLn $ "CNF testing tree result after rewrite   : " <> pass checkCNFtree
  putStrLn $ "CNF testing bools result after rewrite  : " <> pass checkCNFbools
  putStrLn $ "CNF testing string result after rewrite : " <> pass checkCNFstring
  where
    pass v = if and v then "Pass" else "Fail"
