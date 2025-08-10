import Data.Bits (Bits (complement, xor, (.&.), (.|.)))

eval_formula :: [Char] -> Bool
eval_formula = res . go []
  where
    go stack [] = stack
    go stack (c : xc) = go (do_op c stack) xc

    res [e] = e
    res s = error $ "Stack must containe one element at the end : " ++ show s

do_op :: Char -> [Bool] -> [Bool]
do_op '1' x             = True : x
do_op '0' x             = False : x
do_op op [] | op `elem` "!&|^>=" = error $ "Not enough values to do op \'" ++ [op] ++ "\'"
do_op '!' (a : xa)      = complement a : xa
do_op op [_] | op `elem` "&|^>=" = error $ "Not enough values to do op \'" ++ [op] ++ "\'"
do_op '&' (a : b : xab) = (a .&. b) : xab
do_op '|' (a : b : xab) = (a .|. b) : xab
do_op '^' (a : b : xab) = (a `xor` b) : xab
do_op '>' (a : b : xab) = (complement a .|. b) : xab
do_op '=' (a : b : xab) = (a == b) : xab
do_op c _               = error ("Unknown charater \'" ++ [c] ++ "\' found")
