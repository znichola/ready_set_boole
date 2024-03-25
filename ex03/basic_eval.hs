import Data.Bits ( Bits((.|.), (.&.), xor, complement) )

eval_formula :: [Char] -> [Bool]
eval_formula = go []
  where
    go stack [] = stack
    go stack (c : xc) = go (do_op c stack) xc

do_op :: Char -> [Bool] -> [Bool]
do_op '1' x             = True : x
do_op '0' x             = False : x
do_op _ []              = error "no value to do op with"
do_op '!' (a : xa)      = complement a : xa
do_op _ [_]             = error "not enough values to do op"
do_op '&' (a : b : xab) = (a .&. b) : xab
do_op '|' (a : b : xab) = (a .|. b) : xab
do_op '^' (a : b : xab) = (a `xor` b) : xab
do_op '>' (a : b : xab) = (complement a .|. b) : xab
do_op '=' (a : b : xab) = (a == b) : xab
do_op c _               = error ("unknown charater \'" ++ [c] ++ "\' found")
