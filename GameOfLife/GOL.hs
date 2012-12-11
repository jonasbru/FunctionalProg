{-
	********************************************************
    *      HASKELL IMPLEMENTATION OF THE GAME OF LIFE      *
    ********************************************************

    Chalmers -- Functional Programming -- Lab Assignment 4
    Michael Fagno && Jonas Bru
    
    The file was run through hlint without warnings 

    See UIGOL.hs for launching and commands
-}

module GOL (
	nextStep,
	readGrid,
	Cells) where

import Data.List
import Test.QuickCheck
import Data.Maybe
import Parsing
import Control.Arrow


type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Bool

type Cells = [Point]
type Point = (Int,Int)

-- CORE	######################################################################

-- Return the list of neighbors coordinates
getNeighbors :: Point -> [Point]
getNeighbors (x,y) = map ((+) x *** (+) y) neighborsOffset
--getNeighbors (x,y) = map (\(dx,dy) -> (x+dx,y+dy)) neighborsOffset -- same

-- Return the living cells after one step 
nextStep ::  Cells -> Cells
nextStep activeList = map (\(x:xs) -> x) (filter hasToLiveFilter neighborsList)
	where neighborsList = group . sort $ concat [ getNeighbors alive | alive <- activeList];
		  hasToLiveFilter (x:xs) =  evolve x activeList (length (x:xs))

-- Return the next state of a cell according to its neighbors
evolve :: Point -> Cells -> Int -> Bool
evolve point actives 2 | point `elem` actives = True
evolve _ _ 3 = True
evolve point _ _ = False

neighborsOffset = [	(-1,-1),(0,-1),(1,-1),
             				(-1, 0),       (1, 0),
             				(-1, 1),(0, 1),(1, 1)]

-- FILE READER ###############################################################

-- Reads a RLE file and returns the corresponding cells
readGrid :: FilePath -> IO Cells 
readGrid file = do
	f <- readFile file
	let l = lines f --Read lines
	let (l0, l1) = splitAt 1 l -- Get the 1rst line (useless info)
	let size = read (init (words (head l0) !! 2))::Int -- Size of the rows
	let ll = init (concat l1) --Reconcat the other lines, and remove the '!' at the end
	let lin = wordsWhen (=='$') ll --Split the lines by the '$'
	let ret = preTransformLine lin size []			--let ret = [transformLine c size | c <- lin]
	return (getLivingsCoord ret)

-- Helper fct to process the lines
preTransformLine :: [String] -> Int -> Grid -> Grid
preTransformLine [] _ grid = grid
preTransformLine (l0:l1) size grid = 
	preTransformLine l1 size (grid ++ [lRet] ++ plus)
	where (lRet, linesPlus) = transformLine l0 size;
		  plus = replicate linesPlus (replicate size False)

-- Transforms a line from the RLE format into a Row of size s
transformLine :: String -> Int -> (Row Value, Int)
transformLine l s = transformLine' l 0 (replicate s False)

--Parses the string, and modifies the line starting at the Int char
transformLine' :: String -> Int -> Row Value -> (Row Value, Int)
transformLine' [] _ r = (r,0)
transformLine' l s r
    | isJust num =													       -- [1-9][bo$]
      if length (snd (fromJust num)) > 0                                   
        then if head (snd (fromJust num)) == 'o'                           -- [1-9][bo]
          then transformLine' (tail (snd (fromJust num))) (nb + s) newList -- [1-9]o
          else transformLine' (tail (snd (fromJust num))) (nb + s) r       -- [1-9]b
        else (r, nb-1)                                                     -- [1-9]$
    | head l == 'o' = transformLine' (tail l) (s + 1) (r !!= (s, True))    -- o
    | head l == 'b' = transformLine' (tail l) (s + 1) r                    -- b
    | otherwise     =  transformLine' (tail l) s r                         -- ??
    where num     = parse (oneOrMore digit) l
          newList = r !!!= (s, replicate nb True)
          nb      = read (fst (fromJust num))


-- HELPER FCTS ###############################################################
	
-- Changes the element at the given position by the given element in a list.
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) l (i, el) | length l <= i = error ("Index out of bounds !!")
				| i < 0 = error "Negative index !!"
				| otherwise = take i l ++ [el] ++ drop (i+1) l
				
-- Changes the elements at the given position by the given elements.
(!!!=) :: [a] -> (Int,[a]) -> [a]
(!!!=) l (i, el) | length l <= i = error "Index out of bounds !!"
				 | i < 0 = error "Negative index !!"
				 | otherwise = take i l ++ el ++ drop (i+ length el) l

--Split function
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

-- Return coordinate of living cells in a grid
getLivingsCoord :: Grid -> Cells
getLivingsCoord grid = 
	filter (\(a,b)-> grid !! b !! a) (concat [ [ (y,x) | y <- [0..width] ] | x<-[0..heigh] ])
	where heigh = length grid -1;
		  width = length (head grid) - 1