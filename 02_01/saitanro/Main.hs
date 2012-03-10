module Main where

import Data.Sequence ((|>),singleton,ViewL(..),viewl,Seq)
import Control.Applicative ((<$>))
import Control.Monad (when,forM_)
import Control.Monad.ST (runST,ST)
import Data.Array.IO (IOUArray,newArray,writeArray,inRange,freeze,readArray,newArray_)
import Data.Array.Unboxed (UArray,(!),bounds)
import Data.Array.ST (STUArray)
import Data.IORef (newIORef,readIORef,writeIORef)

type Point = (Int,Int)
type Maze = UArray Point Char
type STMaze s = STUArray s Point Int
type IOMaze = IOUArray Point Char

main = do
  n <- getInt
  m <- getInt
  startRef <- newIORef (-1,-1)
  ran <- return $ ((1,1),(n,m))
  maze <- newArray_ ran :: IO IOMaze
  forM_ [1..m] $ \y -> do
    forM_ [1..n] $ \x -> do
      c <- getChar
      writeArray maze (x,y) c
      when (c == 'S') $ writeIORef startRef (x,y)
    getLine
  start <- readIORef startRef
  m <- freeze maze
  print $ solve m start
  where
    getInt :: IO Int
    getInt = read <$> getLine

distances = [(-1,0),(0,-1),(1,0),(0,1)]

bfs :: Maze -> STMaze s -> Seq (Point,Int) -> ST s Int
bfs maze solv queue = case viewl queue of
  EmptyL -> error "do not solve"
  ((n,d) :< queue) ->
    if not $ inRange (bounds maze) n
    then next
    else bfs'
    where
      next = bfs maze solv queue
      c = maze ! n
      bfs' | isGoal c = return d
           | isWall c = next
           | isRoad c = do
             visited <- readArray solv n
             if visited == (-1)
               then do
               let queue' = (foldr addQueue queue $ Prelude.zip (repeat (n,d)) distances)
               writeArray solv n d
               bfs maze solv queue'
               else next
  where
    addQueue (((x,y),d),(dx,dy)) queue = queue |> ((x+dx,y+dy),d+1)

solve :: Maze -> Point -> Int
solve maze start = runST $ do
  let b = bounds maze
  solv <- newArray b (-1) :: ST s (STMaze s)
  bfs maze solv (singleton (start,0))

isGoal :: Char -> Bool
isGoal = (=='G')

isWall :: Char -> Bool
isWall = (=='#')

isRoad :: Char -> Bool
isRoad c = (c=='.') || (c=='S')