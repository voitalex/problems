-----------------------------------------------------------------------------
-- |
-- Module      :  Euler.Common
-- Maintainer  :  voitalexey@gmail.com
-- Description :  Common routines used throughout the Project Euler's 
--                problems
-----------------------------------------------------------------------------

module Euler.Common ( 
   divides, factorial, log10, isPrime, isCoprime, 
   primes, primesL, primesLE, fibs,
   primeFactors, primePowerFactors
) where

import Data.List (group)

--------------------------------------------------------------------------------
-- Math routines
--------------------------------------------------------------------------------

-- | Indicates whether "a" is a divisor of "b".
divides :: (Integral a) => a -> a -> Bool
a `divides` b = b `mod` a == 0


-- | Returns factorial of the argument.
factorial :: Integer -> Integer
factorial = ( product . enumFromTo 1 )


-- | Determines whether two positive integer numbers are coprime. 
-- Two numbers are considered coprime if their greatest common 
-- divisor equals 1.
isCoprime :: (Integral a) => a -> a -> Bool
isCoprime a b = gcd a b == 1


-- | Determines whether the argument is a prime number.
isPrime :: Integer -> Bool
isPrime x =    x > 1 
            && foldr ( \p r -> p * p > x || x `rem` p /= 0 && r ) True primes


-- | Returns decimal logarithm of the argument.
log10 :: Double -> Double
log10 = logBase 10


-- | Determines the prime factors of a given positive integer.
primeFactors :: Integer -> [ Integer ]
primeFactors 0 =  [ ]
primeFactors x =  factor primes $ abs x
                     where
                        factor [ ] _         = [ ]
                        factor ps@(p:pt) n   | p * p > n     = [ n ]               
                                             | rem n p == 0  = p : factor ps (quot n p) 
                                             | otherwise     =     factor pt n


-- |Determines a list containing the prime factors and their multiplicity.
primePowerFactors :: Integer -> [ (Integer, Int) ]
primePowerFactors =  map encode . group . primeFactors
                     where 
                        encode xs = (head xs, length xs)

--------------------------------------------------------------------------------
-- List routines
--------------------------------------------------------------------------------

-- | Combines the result list of two ordered increasing lists
-- and removes dublicates.
union :: (Ord a) => [ a ] -> [ a ] -> [ a ]
union xs  [ ]  = xs
union [ ] ys   = ys  
union (x:xs) (y:ys) = case compare x y of 
                        LT -> x : union  xs  (y:ys)
                        EQ -> x : union  xs     ys 
                        GT -> y : union (x:xs)  ys
           
--------------------------------------------------------------------------------
-- Sequences of numbers
--------------------------------------------------------------------------------

-- | The infinity sequence of Fibonacci numbers.
fibs :: [ Integer ]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


-- | The sequence of Pascal's triangle values.
pascal :: [ [ Integer] ]
pascal =  iterate ( \row -> zipWith (+) ( [0] ++ row ) ( row ++ [0] ) ) [ 1 ]


-- | Tree-merging Eratosthenes sieve that produces 
-- infinite list of all prime numbers.
primes :: [ Integer ]
primes = 2 : gaps 3 ( join [ [p * p, p * p + 2 * p .. ] | p <- primes' ] )
         where
            primes'             = 3 : gaps 5 ( join [ [p * p, p * p + 2 * p .. ] | p <- primes' ] )
            join  ((x:xs):t)    = x : union xs ( join (pairs t) )
            pairs ((x:xs):ys:t) = (x : union xs ys) : pairs t
            gaps k xs@(x:t)   | k == x      = gaps ( k + 2 ) t 
                              | otherwise   = k : gaps ( k + 2 ) xs
    

-- | The sequence of prime numbers less than the value provided.
primesL :: Integer -> [ Integer ]
primesL n   = takeWhile ( < n ) primes


-- | The sequence of prime numbers not greater than the value provided.
primesLE :: Integer -> [ Integer ]
primesLE n  = takeWhile ( <= n ) primes

