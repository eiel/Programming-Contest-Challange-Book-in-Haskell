module Main where

import Control.Applicative
import Control.Monad
-- Data.Map.deleteFindMin $ Data.Map.insert 5 5 $ Data.Map.insert 20 20 $ Data.Map.insert 10 10 Data.Map.empty

main = do
  n <- getInt
  l <- forM [1..n] $ \_ -> getInt
  print $ solve l
  where
    getInt :: IO Int
    getInt = read <$> getLine

-- 小さな値を二つみつけてきて、みつけてきた値ふたつを合計し、記録していく。その値をリストに追加したあとで再度追加してをくりかえす。
type Nums = [Int]

solve :: Nums -> Int
solve xs = solve' xs 0
  where
    solve' :: Nums -> Int -> Int
    solve' [] a = a
    solve' (_:[]) a = a
    solve' xs a = case min' xs of
      (min1,min2,xs') -> solve' (m:xs') $ m+a
        where
          m = min1+min2

min' :: Nums -> (Int, Int, Nums)
min' (x1:x2:xs) = foldr f (x1,x2,[]) xs
  where
    f x' (min1,min2,xs') = if min1 > x'
                           then (x',min1,min2:xs')
                           else if min2 > x'
                                then (min1,x',min2:xs')
                                else (min1,min2,x':xs')

answer = solve [8,5,8]