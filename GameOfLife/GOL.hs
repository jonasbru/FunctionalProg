module GOL where

import Data.List
import Test.QuickCheck
import Debug.Trace
import System.Console.ANSI
import Control.Concurrent
import Graphics.GD

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Bool
type Point = (Int,Int)

-- Micka	####################################################################
-- create Image 	############################################################

-- Loop 	####################################################################
run grid = 	do
							printGrid grid
							threadDelay(1000000)
							run (nextStep grid)

-- Core #####################################################################
-- Return the next state of a grid
nextStep ::  Grid -> Grid
nextStep grid = [ [ transform (x,y) grid | y <-[0..nbRows-1] ] | x <-[0..nbLines-1] ]
								where	nbLines = length grid;
											nbRows = length $ head grid

-- Return the list of neighbors coordinates
getNeighbors :: Point -> Grid -> [Point]
getNeighbors (x,y) grid = filter (\a -> isInside a grid) neighbor
												where neighbor = map (\(dx,dy) -> (x+dx,y+dy)) neighborsOffset

-- Check if a point is in the grid
isInside:: Point -> Grid -> Bool
isInside (a,b) grid | a >= 0 && a < length grid && b >= 0 && b < length (head grid) = True
										| otherwise = False

neighborsOffset = [	(-1,-1),(0,-1),(1,-1),
             				(-1, 0),       (1, 0),
             				(-1, 1),(0, 1),(1, 1)]

-- Count how many cells are alive near a cell
countNextAlive :: Point -> Grid -> Int
countNextAlive pt grid = length $ filter (\(x,y)->grid!!x!!y==True) (getNeighbors pt grid)

-- Return true if a cell has to die
hasToDie :: Point -> Grid -> Bool
hasToDie pt grid 	| nbAlive  < 2 || nbAlive > 3= True
									| otherwise = False
									where nbAlive = countNextAlive pt grid

-- Return true if a cell has to born
hasToBorn :: Point -> Grid -> Bool
hasToBorn pt grid | nbAlive  == 3 = True
									| otherwise = False
									where nbAlive = countNextAlive pt grid

-- return the next state of a cell 
transform:: Point -> Grid -> Bool
transform (x,y) grid	| hasToDie pt grid = False
											| hasToBorn pt grid = True
											| otherwise = grid!!x!!y
												where pt = (x,y)
-- End Core ##################################################################

-- Jonas #####################################################################


-- Terminal print ############################################################
printGrid :: Grid -> IO ()
printGrid g = 
	do 
		clearScreen
		mapM_ putStrLn lines
	where lines = map listToString g

listToString :: Row Value -> String
listToString list = [b | b <- map toString list]

toString :: Bool -> Char
toString True = 'o'
toString False = ' '

-- File reader ###############################################################

--readGrid :: FilePath -> IO Grid 
{-
readGrid file = do 
	f <- readFile file
	l <- lines f
	(l0, l1) <- splitAt 1 l
	ll <- concat l1
	return ll 
-}

--readGrid :: FilePath -> Grid		
{-
readGrid file = 
	ll
	where
		f = do
			merdeuh <- readF file
			let cacaca = merdeuh
			return cacaca
		l = lines f;
		(l0, l1) = splitAt 1 l;
		ll = concat l1
	-}	
readF :: FilePath -> IO String
readF file = do 
	p <- readFile file
	return p
	
--	let s = Sudoku [map transformLine p | p <- lines g]
	
-- Returns the case corresponding to the char
--transformLine :: Char -> Maybe Int
--transformLine '.' = Nothing
--transformLine c = Just (digitToInt c)

------------------------------------------------
example=[[False,False, False],[True,True, True],[False,False, False]]
plop = [[True,False],[False,True]]
plop2 = [[False,True],[True,False]]
