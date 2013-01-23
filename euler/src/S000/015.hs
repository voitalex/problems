--------------------------------------------------------------------------------
-- %author  Alex Voytekhovsky
-- %task    015
-- %desc    Starting in the top left corner of a 2x2 grid, there are 6 routes
--          (without backtracking) to the bottom right corner.
--          How many routes are there through a 20x20 grid?
-- %usage   el015 20 20
--------------------------------------------------------------------------------

import Data.Function (on)
import Common (factorial)

el015 a b = x `div` y
            where
               x = factorial (a + b)
               y = ( (*) `on` factorial ) a b 