--------------------------------------------------------------------------------
-- %author  Alex Voytekhovsky
-- %task    025
-- %desc    The Fibonacci sequence is defined by the recurrence relation:
--             Fn = Fn-1 + Fn-2, where F1 = 1 and F2 = 1.
--          The 12th term, F12, is the first term to contain three digits.
--          What is the first term in the Fibonacci sequence 
--          to contain 1000 digits?
-- %usage   el025 1000
--------------------------------------------------------------------------------

import Common (fibs)

el025 n = fst $ head $ dropWhile (\(x,y) -> y < 10^(n-1) ) $ zip (enumFrom 0) fibs