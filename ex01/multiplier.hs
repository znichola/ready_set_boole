module Multiplier where

import Data.Bits (shiftL, shiftR, xor, (.&.))
import Data.Word (Word32)

multiplier :: Word32 -> Word32 -> Word32
multiplier = go 0
  where
    go res _ 0 = res
    go res 0 _ = res
    go res a b = go (if b .&. 1 == 1 then adder res a else res) (shiftL a 1) (shiftR b 1)

adder :: Word32 -> Word32 -> Word32
adder = go
  where
    go a 0 = a
    go a b = go (xor a b) (shiftL carry 1) where carry = a .&. b

-- reference impletmentation

-- unsigned int mult(x, y)
-- unsigned int x, y;
-- {
--     unsigned int reg = 0;

--     while (y != 0)
--     {
--         if (y & 1)
--         {
--             reg += x;
--         }
--         x <<= 1;
--         y >>= 1;
--     }
--     return reg;
-- }
