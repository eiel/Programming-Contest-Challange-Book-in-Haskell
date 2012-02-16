module Main where
import Debug.Trace
import Control.Monad
import Control.Monad.ST
import Control.Monad.State
import Data.Array.ST
import Data.Array.IO
import Data.Array.Unboxed

type Field = Array (Int,Int) Char
type STField s = STUArray s (Int, Int) Char
type IOField = IOUArray (Int,Int) Char
type Point = (Int,Int)

main = do
  n <- getInt
  m <- getInt
  field <- newArray ((1,1),(n,m)) ' ' :: IO IOField
  forM_ [1..n] $ \x -> do
    forM_ [1..m] $ \y -> do
      c <- getChar
      writeArray field (x,y) c
    getChar
  f <- freeze field
  print $ solve f
  where
    getInt :: IO Int
    getInt = fmap read $ getLine


dfs :: Field -> Point -> Field
dfs f (x,y) = round $ f // [((x,y), '.')]
  where
    ((xs,ys),(xe,ye)) = bounds f
    inRange x y = (x >= xs && x <= xe && y >= ys && y <= ye)
    isSame x1 y1 = x == x1 && y == y1
    ixs = [(x1,y1)| x1 <- [x-1,x,x+1],
           y1 <- [y-1,y,y+1],
           not (isSame x1 y1) && inRange x1 y1 && isWater (f ! (x1,y1))]
    round f' = foldr (\p f'' -> dfs f'' p) f' ixs

solve :: Field -> Int
solve f = case foldr solve' (f,0) xys of (_,r) -> r
  where
    ((xs,ys),(xe,ye)) = bounds f
    xys = [(x,y)| x <- [xs..xe],y  <- [ys..ye]]
    solve' :: (Int,Int) -> (Field,Int) -> (Field,Int)
    solve' (x,y) (f,r) = if isWater $ f ! (x,y)
                     then (dfs f (x,y),r+1)
                     else (f,r)

isWater :: Char -> Bool
isWater = (=='W')

ar :: Field
ar = array ((1,1),(2,2)) [((1,1), 'W'),((2,2), 'W')]