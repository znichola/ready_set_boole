module Adder where

import Data.Bits
import Data.Foldable
import Data.Int
import Data.Word

-- printBits :: Int8 -> IO ()
-- printBits x = putStrLn (showFiniteBits x)

adder :: Int32 -> Int32 -> Int32
adder = go
  where
    go a 0 = a
    go a b = go (xor a b) (shiftL carry 1) where carry = a .&. b

-- until' p f = go
--   where
--     go x
--       | p x = x
--       | otherwise = go (f x)

-- def Bitwise_add(a,b):
--     while b != 0:
--         carry = a&b # Carry value is calculated
--         a = a^b # Sum value is calculated and stored in a
--         b = carry<<1 # The carry value is shifted towards left by a bit
--     return a # returns the final sum

-- Utility functions for printing binary reprisenttion

getBitsArray :: Int32 -> [Bool]
-- implemented with a lambda expression for the function
getBitsArray x = fmap (\pos -> 0 /= x .&. shiftL 1 pos) [31, 30 .. 0]

-- implemented using chain opetator
-- getBitsArray x = fmap ((0 /=) . (x .&.) . shiftL 1) [31, 30 .. 0]

getBits :: Int32 -> String
getBits x = fmap (showBitAt x) [31, 30 .. 0]
  where
    showBitAt x pos = if (x .&. shiftL 1 pos) /= 0 then '1' else '0'
