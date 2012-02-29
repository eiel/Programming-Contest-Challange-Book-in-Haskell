module Main where

import Control.Monad (forM)
import Text.Printf (printf)

main = do
  n <- getInt
  a <- forM [1..n] $ \n ->
    getInt
  k <- getInt
  printSolve $ solve a k
    where
      getInt :: IO Int
      getInt = fmap read $ getLine
      printSolve (n,s) = if n
                         then printf "YES (%s)\n" s
                         else putStrLn "NO"

type Sum = Int
type Inputs = [Int]
type Result = [Int]

solve :: Inputs -> Int -> (Bool,String)
solve a k = case dfs 0 a [] of
  Just r -> (True,format k r)
  Nothing -> (False,"")
  where
    dfs :: Sum -> Inputs -> Result -> Maybe Result
    dfs sum [] r = if sum == k
                   then Just r
                   else Nothing
    dfs sum (x:xs) r = case dfs (sum+x) xs (x:r) of
      Just r -> Just r
      Nothing -> dfs sum xs r

format :: Int -> Result -> String
format k xs = show k ++ " = " ++ (format' xs)
  where
    foramt' [] = ""
    format' (x:[]) = show x
    format' (x:xs) = show x ++ " + " ++ format' xs
