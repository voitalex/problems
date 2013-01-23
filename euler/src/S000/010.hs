--------------------------------------------------------------------------------
-- %author  Alex Voytekhovsky
-- %task    010
-- %desc    The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--          Find the sum of all the primes below two million.
-- %usage   el010 2000000
--------------------------------------------------------------------------------

import Common (primes)

el010 n = sum $ takeWhile ( < n ) $ primes