module Adder where

import Data.Bits
import Data.Word

-- haskell Word32 is equal to c unsigned long

adder :: Word32 -> Word32 -> Word32
adder = go
  where
    go a 0 = a
    go a b = go (xor a b) (shiftL carry 1) where carry = a .&. b

-- reference impletmentation

-- def Bitwise_add(a,b):
--     while b != 0:
--         carry = a&b
--         a = a^b
--         b = carry<<1
--     return a

-- Utility functions for printing binary reprisenttion

getBitsArray :: Word32 -> [Bool]
-- implemented with a lambda expression for the function
getBitsArray x = fmap (\pos -> 0 /= x .&. shiftL 1 pos) [31, 30 .. 0]

-- implemented using chain opetator
-- getBitsArray x = fmap ((0 /=) . (x .&.) . shiftL 1) [31, 30 .. 0]

getBits :: Word32 -> String
getBits x = fmap (showBitAt x) [31, 30 .. 0]
  where
    showBitAt x pos = if (x .&. shiftL 1 pos) /= 0 then '1' else '0'
