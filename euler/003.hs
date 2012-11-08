--------------------------------------------------------------------------------
-- %author  Alex Voytekhovsky
-- %task    003
-- %desc    The prime factors of 13195 are 5, 7, 13 and 29.
--          What is the largest prime factor of the number 600851475143 ?
-- %usage   el003 600851475143
--------------------------------------------------------------------------------

el003 n =   largestPrimeFactor n
            where
               largestPrimeFactor n = factorOut n 2
               factorOut n i  | n <= i         = n
                              | n `mod` i == 0 = factorOut (n `div` i) i
                              | otherwise      = factorOut n          (i + 1)