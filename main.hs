{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
-- highest is pretty easy to get - we're always going to end up at the LCM of 1..n
-- if you go in in order, then everyone's unhappy up to n
-- but then all bets are off. damn.

-- so at that point, we need all the common multiples of all our numbers, sorted and uniqed.

-- can't get to primes > n, because no-one would go there anyway.
-- so: all products of the power set of primes < n, raised to an arbitrary power.
import Data.Numbers.Primes
import Data.List
import Logs
import Debug.Trace
import Data.Array
import Control.Parallel
import Control.Parallel.Strategies
import Data.Word
import qualified Data.Vector.Unboxed as U
import Data.Vector.Binary
import Data.Binary

solve :: U.Vector Word -> Word -> Word
solve !smallprimes !no = diff small_arr
  where diff (!x) = {-# SCC "diff" #-} 1 + U.foldl' (\(!acc) (!x) -> acc + (diff_i x)) 0 x
        small_arr = {-# SCC "arr" #-} U.takeWhile (<= (ceiling . sqrt $ fromIntegral no))  smallprimes
        diff_i (!x) = {-# SCC "diff_i" #-} (intlog no x ) - 1

-- repa stuff
-- small_array = fromFunction (Z :. len) (\(Z :. ix) -> arr ! (Z :. ix))
-- arr_primes = {-# SCC "arr_primes" #-} fromList (Z :. len :: DIM1) smallerprimes
 -- diff = {-# SCC "diff" #-} (+1) . flip (Repa.!) Z . Repa.fold (\acc -> (+acc) . diff_i) 0
main = do
  primes <- {-# SCC "decode" #-} decodeFile "primes.dat" :: IO (U.Vector Word)
  l <- getContents
  putStr . unlines .  format . parMap rseq (solve primes .read) .  tail $ lines l

format n = map (\(num, s) -> "Case #" ++ show num  ++ ": " ++ show s  ) $ zip [1..] n
