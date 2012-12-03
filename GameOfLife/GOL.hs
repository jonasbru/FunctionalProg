module GOL where

import Data.List
import Test.QuickCheck
import Debug.Trace

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Bool
type Point = (Int,Int)

getNeighbors :: Point -> Grid -> [Point]
getNeighbors (x,y) grid = filter (\a -> isInside a grid) neighbor
												where neighbor = map (\(dx,dy) -> (x+dx,y+dy)) neighborsOffset

isInside (a,b) grid | a >= 0 && a < length grid && b >= 0 && b < length (head grid) = True
										| otherwise = False

neighborsOffset = [(-1,-1),(0,-1),(1,-1),
             (-1, 0),       (1, 0),
             (-1, 1),(0, 1),(1, 1)]

 

example=[[False,True, False],[False,True, False],[False,True, False]]
