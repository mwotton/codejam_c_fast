import Data.Numbers.Primes
import Data.Word
import Data.Binary
import Data.Vector.Binary
import qualified Data.Vector.Unboxed as U
biggest::Integer
biggest = 10^12

smallprimes :: [Word]
smallprimes  = {-# SCC "smallprimes" #-} takeWhile (<= (ceiling . sqrt $ fromIntegral biggest))  primes

small_vector = U.fromList smallprimes

main = encodeFile "primes.dat"  small_vector
