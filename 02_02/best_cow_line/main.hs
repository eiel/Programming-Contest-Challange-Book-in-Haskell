module Main where

import Data.Sequence as S
import Control.Applicative
import Control.Monad

type Length = Int
type Word = Seq Char

main = do
  len <- getInt
  word <- foldM getter S.empty [1..len]
  solve word
  where
    getter :: Word -> Int -> IO Word
    getter xs _ = do
      c <- getChar
      return $ xs |> c
    getInt :: IO Int
    getInt = read <$> getLine

solve :: Word -> IO ()
solve w = if S.null w
          then putChar '\n'
          else case solve' w of
            (x,w) -> do
              putChar x
              solve w

solve' :: Word -> (Char,Word)
solve' s = case compare' (viewl s) (viewr s) of
  Left (x :< ss) -> (x,ss)
  Right (ss :> x) -> (x,ss)

compare' ::  ViewL Char -> ViewR  Char -> Either (ViewL Char) (ViewR Char)
compare' l EmptyR = Left l
compare' EmptyL r = Right r
compare' l@(x:<xs) r@(ys:>y) = case compare x y of
  LT -> Left l
  GT -> Right r
  EQ -> case compare' (viewl xs) (viewr ys) of
    Left x -> Left l
    Right x -> Right r


check_solve s = check_solve' (solve' $ S.fromList s) []
  where
    check_solve' (x,w) xs = if S.null w
                            then x:xs
                            else check_solve' (solve' w) $ x:xs


input_str = "ACDBCB"
input_length = Prelude.length input_str
answer = "ABCBCD"

check = check_solve input_str == Prelude.reverse answer
