
import Control.Monad( when )

main = do
   num <- getLine
   when( num /= "42" ) $ do
      putStrLn num
      main
