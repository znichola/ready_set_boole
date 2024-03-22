{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module Gray_code where

import Data.Binary (Word32)
import Data.Bits (shiftR, xor)

grey_code :: Word32 -> Word32
grey_code num = xor num (shiftR num 1)

-- reference implementation

-- uint BinaryToGray(uint num)
-- {
--   return num ^ (num >> 1);
-- }
