module Main where

import Control.Monad (forM)
import Debug.Trace
import Text.Printf

main = do
  cs <- forM [1..6] $ \_ -> getInt
  a <- getInt
  print $ solve cs a
    where
      getInt :: IO Int
      getInt = fmap read $ getLine

solve :: [Int] -> Int -> Int
solve cs a = snd $ foldr solve' (a,0) $ zip cs vs
  where
    solve' :: (Int, Int) -> (Int, Int) -> (Int, Int)
    solve' (c,v) (a',t') = (a' - (t * v), t'+t)
      where
        t = min (a' `div` v) c

vs = [1, 5, 10, 50, 100, 500]

solve01 = solve [3, 2, 1, 3, 0, 2] 620
