module Main where

import Control.Monad (forM)

main = do
  n <- getInt
  ss <- forM [1..n] $ \n -> getInt
  ts <- forM [1..n] $ \n -> getInt
--  print ss
--  print ts
  print $ solve ss ts 0 0
    where
      getInt :: IO Int
      getInt = fmap read $ getLine

-- ルール：選べる仕事の中で、終了時間が最も早いものを選ぶことを繰り返す
solve :: [Int] -> [Int] -> Int -> Int -> Int
solve [] [] ans end = ans
--solve (s:ss) (t:ts) ans end = if end < s then solve ss ts (ans + 1) t
--                              else solve ss ts ans end
solve (s:ss) (t:ts) ans end | end < s = solve ss ts (ans + 1) t
                            | otherwise = solve ss ts ans end

solve' = solve [1, 2, 4, 6, 8] [3, 5, 7, 9, 10] 0 0
