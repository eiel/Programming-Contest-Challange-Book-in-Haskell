module Main where

import Control.Monad (forM_,when)
import Control.Monad.ST (ST,runST)
import Data.Array.ST (STUArray)
import Data.Array.IO (freeze,thaw,writeArray,getBounds,readArray,newArray,IOUArray)
import Data.Array.Unboxed (UArray,bounds,array)
import Data.Ix (inRange,range)
import Data.STRef (newSTRef,readSTRef,writeSTRef)

type Point = (Int,Int)
type Field = UArray Point Char
type STField s = STUArray s Point Char
type IOField = IOUArray Point Char

main = do
  n <- getInt
  m <- getInt
  let r = ((1,1),(n,m))
  field <- newArray r ' ' :: IO IOField
  forM_ [1..n] $ \x -> do
    forM_ [1..m] $ \y -> do
      c <- getChar
      writeArray field (x,y) c
    getChar
  f <- freeze field
  print . solve $ f
  where
    getInt :: IO Int
    getInt = fmap read $ getLine

-- もどり値にSTがつくように配列の値を書き換える操作がある
dfs :: STField s -> Point -> ST s ()
dfs f p@(x,y) = do
  writeArray f p '.'
  b <- getBounds f
  forM_ r $ \p1 -> do
    when (not $ isSame p1) $ do
      when (inRange b p1) $ do
        c <- readArray f p1
        when (isWater c) $ do
          dfs f p1
  where
    isSame (x',y') = x == x' && y == y'
    r = range ((x-1,y-1),(x+1,y+1))

-- 破壊的な操作をするが、その影響は外には出さない。
-- 一度だけ配列全体のコピーを行なう(thaw)
solve :: Field -> Int
solve field = runST $ do
      f <- thaw field :: ST s (STField s)
      solve' f
        where
          solve' :: STField s -> ST s Int
          solve' f = do
            ref <- newSTRef 0
            forM_ r $ \p -> do
              c <- readArray f p
              when (isWater c) $ do
                dfs f p
                x <- readSTRef ref
                writeSTRef ref (x+1)
            readSTRef ref
          r = range $ bounds $ field

isWater :: Char -> Bool
isWater = (=='W')

-- サンプルデータ
ar :: Field
ar = array ((1,1),(2,2)) [((1,1), 'W'),((2,2), 'W')]