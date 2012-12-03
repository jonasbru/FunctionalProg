module GOL where

import Data.List
import Test.QuickCheck
import Debug.Trace

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Bool
type Point = (Int,Int)

nextStep ::  Grid -> Grid
nextStep grid = [ [ transform (x,y) grid | y <-[0..nbRows-1] ] | x <-[0..nbLines-1] ]
								where	nbLines = length grid;
											nbRows = length $ head grid

getNeighbors :: Point -> Grid -> [Point]
getNeighbors (x,y) grid = filter (\a -> isInside a grid) neighbor
												where neighbor = map (\(dx,dy) -> (x+dx,y+dy)) neighborsOffset

isInside (a,b) grid | a >= 0 && a < length grid && b >= 0 && b < length (head grid) = True
										| otherwise = False

neighborsOffset = [	(-1,-1),(0,-1),(1,-1),
             				(-1, 0),       (1, 0),
             				(-1, 1),(0, 1),(1, 1)]

countNextAlive :: Point -> Grid -> Int
countNextAlive pt grid = length $ filter (\(x,y)->grid!!x!!y==True) (getNeighbors pt grid)

hasToDie pt grid 	| nbAlive  < 2 || nbAlive > 3= True
									| otherwise = False
									where nbAlive = countNextAlive pt grid

hasToBorn pt grid | nbAlive  == 3 = True
									| otherwise = False
									where nbAlive = countNextAlive pt grid

transform:: Point -> Grid -> Bool
transform (x,y) grid	| hasToDie pt grid = False
											| hasToBorn pt grid = True
											| otherwise = grid!!x!!y
												where pt = (x,y)

---------------------------------------------------------------------
example=[[False,False, False],[True,True, True],[False,False, False]]
