--------------------------------------------------------------------------------
-- %author  Alex Voytekhovsky
-- %task    005
-- %desc    2520 is the smallest number that can be divided by each of 
--          the numbers from 1 to 10 without any remainder.
--          What is the smallest positive number that is evenly divisible by 
--          all of the numbers from 1 to 20?
-- %usage   el005 20
--------------------------------------------------------------------------------

import Common ( primesLE, log10 )
           
el005 m =   product [ a ^ (f a) | a <- primesLE m ]
            where
               logM     = log10( fromIntegral m )
               f x      = floor( logM / ( log10( fromIntegral x ) ) )