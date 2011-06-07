{-# LANGUAGE BangPatterns #-}
module Logs where
import Data.Word
import Debug.Trace


intlog :: Word -> Word -> Word
--intlog 1 _ = error "don't be an idiot"
--intlog _ 1 = 0
intlog !my_number !my_base = gobble my_number 0
  where
     gobble !internal_number !accumulator
         | internal_number < my_base = accumulator
         | otherwise = gobble (div internal_number my_base) (succ accumulator)

