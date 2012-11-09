--------------------------------------------------------------------------------
-- %author  Alex Voytekhovsky
-- %task    009
-- %desc    A Pythagorean triplet is a set of three natural numbers, 
--          a < b < c, for which, a^2 + b^2 = c^2
--          For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
--          There exists exactly one Pythagorean triplet for which 
--          a + b + c = 1000.
--          Find the product abc.
-- %usage   el009 1000
--------------------------------------------------------------------------------

el009 n =   head $ [ x * y * (n - x - y) | let l = enumFromTo 1 n, x <- l, y <- l, f x y ]
            where
               f x y = 2 * x * y - 2 * n * (x + y) + n * n == 0