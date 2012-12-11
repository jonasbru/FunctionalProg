module GOL (knextStep,getLivingsCoord,Cells) where

{- 

You will need to run the followings commands : 
$> cabal update 
$> cabal install ansi-terminal

-}

import Data.List
import Test.QuickCheck
import Debug.Trace
import System.Console.ANSI
import Data.Maybe
import Parsing
import Control.Concurrent


type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Bool

-- Micka	####################################################################
type Cells = [Point]
type Point = (Int,Int)

-- Return the list of neighbors coordinates
kgetNeighbors :: Point -> [Point]
kgetNeighbors (x,y) = map (\(dx,dy) -> (x+dx,y+dy)) neighborsOffset

knextStep ::  Cells -> Cells
knextStep activeList = map (\(x:xs) -> x) (filter (hasToLiveFilter) neighborsList)
					where neighborsList = group . sort $ concat ([ kgetNeighbors alive | alive <- activeList]);
								hasToLiveFilter (x:xs) =  kevolve x activeList (length (x:xs))

kevolve :: Point -> Cells -> Int -> Bool
kevolve point actives 2 | elem point actives= True
kevolve _ _ 3 = True
kevolve point _ _ = False

neighborsOffset = [	(-1,-1),(0,-1),(1,-1),
             				(-1, 0),       (1, 0),
             				(-1, 1),(0, 1),(1, 1)]

-- ****************************************************************************
-- Loop 	####################################################################
run grid = 	do
							printGrid grid
							threadDelay(100000)
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

--neighborsOffset = [	(-1,-1),(0,-1),(1,-1),
--             				(-1, 0),       (1, 0),
--             				(-1, 1),(0, 1),(1, 1)]

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

-- return coordinates of living cells
getLivingsCoord :: Grid -> [Point]
getLivingsCoord grid = filter (\(a,b)->grid!!a!!b==True ) (concat [ [ (x,y) | y <- [0..width] ] | x<-[0..heigh] ])
										where heigh = (length grid) -1;
													width = (length $ head grid) -1
	
-- End Core ##################################################################

-- Jonas #####################################################################

addRows :: Grid -> Int -> Grid
addRows g i = g ++ (replicate i (replicate (length (g!!0)) False))

addCols :: Grid -> Int -> Grid
addCols g i = transpose ((transpose g) ++ (replicate i (replicate (length ((transpose g)!!0)) False)))


-- Terminal print ############################################################
printGrid :: Cells -> IO ()
printGrid g = 
	do 
		clearScreen
		putStr (unlines lines) --mapM_ putStrLn lines
	where lines = map listToString g

listToString :: Row Value -> String
listToString list = [b | b <- map toString list]

toString :: Bool -> Char
toString True = 'O'
toString False = '.'

-- File reader ###############################################################

readGrid :: FilePath -> IO Grid 
readGrid file = do
	f <- readFile file
	let l = lines f --Read lines
	let (l0, l1) = splitAt 1 l -- Get the 1rst line (useless info)
	let size = read (init ((words (l0!!0)) !! 2))::Int -- Size of the rows
	let ll = init (concat l1) --Reconcat the other lines, and remove the '!' at the end
	let lin = wordsWhen (=='$') ll --Split the lines by the '$'
	let ret = [transformLine c size | c <- lin]
	return ret
	
transformLine :: String -> Int -> Row Value
transformLine l s = transformLine' l 0 (replicate s False)

--Parses the string, and modifies the line starting at the Int char
transformLine' :: String -> Int -> Row Value -> Row Value
transformLine' [] _ r = r
transformLine' l s r = 
	if isJust num 
		then if length (snd (fromJust num)) > 0 
				then if (snd (fromJust num)) !! 0 == 'o' 
						then transformLine' (tail (snd (fromJust num))) (nb + s) newList
						else transformLine' (tail (snd (fromJust num))) (nb + s) r
				else r
		else if l !! 0 == 'o'
				then transformLine' (tail l) (s + 1) (r !!= (s, True))
				else transformLine' (tail l) (s + 1) r
	where 
		num = parse (oneOrMore digit) l;
		newList = r !!!= (s, (replicate nb True));
		nb = read (fst (fromJust num))
	

-- Changes the element at the given position by the given element in a list.
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) l (i, el) | length l <= i = error "Index out of bounds !!"
				| i < 0 = error "Negative index !!"
				| otherwise = take i l ++ [el] ++ drop (i+1) l
				
-- Changes the elements at the given position by the given elements.
(!!!=) :: [a] -> (Int,[a]) -> [a]
(!!!=) l (i, el) | length l <= i = error "Index out of bounds !!"
				| i < 0 = error "Negative index !!"
				| otherwise = take i l ++ el ++ drop (i+(length el)) l

--Split function
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

------------------------------------------------
example=[(2::Int,0::Int),(2::Int,1::Int),(2::Int,3::Int)]

