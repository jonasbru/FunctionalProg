module GOL where

import Data.List
import Test.QuickCheck
import Debug.Trace
import System.Console.ANSI

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Bool
type Point = (Int,Int)

getNeighbors :: Point -> Grid -> [Point]
getNeighbors (x,y) grid = filter (\a -> isInside a grid) neighbor
												where neighbor = map (\(dx,dy) -> (x+dx,y+dy)) neighborsOffset

rows :: Matrix a -> [Row a]
rows = id

-- Micka #####################################################################
isInside (a,b) grid | a >= 0 && a < length grid && b >= 0 && b < length (head grid) = True
										| otherwise = False

neighborsOffset = [(-1,-1),(0,-1),(1,-1),
             (-1, 0),       (1, 0),
             (-1, 1),(0, 1),(1, 1)]

 

example=[[False,True, False],[False,True, False],[False,True, False]]


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
plop = [[True,False],[False,True]]
plop2 = [[False,True],[True,False]]


