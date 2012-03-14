module Main where

import Control.Monad (forM)
import Debug.Trace
import Text.Printf

main = do
  cs <- forM [1..6] $ \_ -> getInt
  a <- getInt
  print cs
  print a
  print $ solve cs a
    where
      getInt :: IO Int
      getInt = fmap read $ getLine

solve :: [Int] -> Int -> Int
solve cs a = solve' (reverse $ zip cs vs) a 
  where
    solve' :: [(Int, Int)] -> Int -> Int
    solve' [] _ = 0
    solve' ((c,v):vcs) a' = t + solve' vcs (a' - (t * v))
      where
        t = min (a' `div` v) c

vs = [1, 5, 10, 50, 100, 500]

