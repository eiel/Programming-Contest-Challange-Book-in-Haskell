module Main where

import Control.Monad (forM)
import Data.List (sort)

main = do
  n <- getInt
  ss <- forM [1..n] $ \n -> getInt
  ts <- forM [1..n] $ \n -> getInt
  print $ solve (sort' ss ts) 0 0
    where
      getInt :: IO Int
      getInt = fmap read $ getLine

-- ルール：選べる仕事の中で、終了時間が最も早いものを選ぶことを繰り返す
solve :: [(Int, Int)] -> Int -> Int -> Int
solve [] ans end = ans
solve ((t, s):ss) ans end | end < s = solve ss (ans + 1) t
                                | otherwise = solve ss ans end
sort' :: [Int] -> [Int] -> [(Int, Int)]
sort' ss ts = sort $ zip ts ss

solve' = solve (sort' [2, 1, 4, 6, 8] [5, 3, 7, 9, 10]) 0 0
