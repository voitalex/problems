--------------------------------------------------------------------------------
-- %author  Alex Voytekhovsky
-- %task    004
-- %desc    A palindromic number reads the same both ways. The largest 
--          palindrome made from the product of two 2-digit numbers is 
--          9009 = 91 * 99.
--          Find the largest palindrome made from the product of two 
--          3-digit numbers.
-- %usage   el004 100 999
--------------------------------------------------------------------------------

import Common (toInt)

maxPalindrome :: [Int] -> Maybe Int
maxPalindrome [] = Nothing
maxPalindrome xs = Just (maximum xs)

el004 :: Int -> Int -> Maybe Int
el004 l r = maxPalindrome $ map toInt $ filter isPalindrome $ map show lst 
            where
               isPalindrome s = s == reverse s 
               lst = [ x * y | let xs = enumFromTo l r, x <- xs, y <- xs ]
