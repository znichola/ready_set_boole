import Data.List (length, sortBy, uncons)
import Debug.Trace (trace)

powerset :: [Int] -> [[Int]]
powerset [] = [[]]
powerset (e : t) =
  let t' = powerset t
   in t' <> [e : e' | e' <- t']

powerset' s = sortBy compare' $ powerset s

compare' x y =
  let x' = length x
      y' = length y
   in if x' == y' then compare (uncons x) (uncons y) else compare x' y'

powersetInput =
  [ [],
    [0],
    [0, 1],
    [0, 1, 2],
    [-1, 23]
  ]

powersetOutput =
  [ [[]],
    [[], [0]],
    [[], [0], [1], [0, 1]],
    [[], [0], [1], [2], [0, 1], [0, 2], [1, 2], [0, 1, 2]],
    [[], [-1], [23], [-1, 23]]
  ]

checkSat =
  zipWith
    ( \x y ->
        (powerset' x == y)
          || trace
            ("\nEXPECTED\n" ++ show y ++ "\nACTUAL\n" ++ show (powerset' x))
            False
    )
    powersetInput
    powersetOutput

runTests = do
  putStrLn $ "testing sat for previous examples : " <> pass checkSat
  where
    pass v = if and v then "Pass" else "Fail"
