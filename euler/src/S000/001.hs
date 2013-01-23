--------------------------------------------------------------------------------
-- %author  Alex Voytekhovsky
-- %task    001
-- %desc    If we list all the natural numbers below 10 that are multiples of 
--          3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
--          Find the sum of all the multiples of 3 or 5 below 1000.
-- %usage   el001 1000
--------------------------------------------------------------------------------
   
el001 n =   sum $ filter (multipleOf [3, 5]) $ enumFromTo 1 (n - 1)
            where
               multipleOf ns x = any (== 0) $ map ( x `mod` ) ns


