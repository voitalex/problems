--------------------------------------------------------------------------------
-- %author  Alex Voytekhovsky
-- %task    007
-- %desc    By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, 
--          we can see that the 6th prime is 13.
--          What is the 10 001st prime number?
-- %usage   el007 10001
--------------------------------------------------------------------------------

import Common (primes)

el007 n = primes !! (n-1)