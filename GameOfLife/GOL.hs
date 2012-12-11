module GOL (
	nextStep,
	readGrid,
	preTransformLine,
	Cells) where

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
import Control.Arrow


type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Bool

-- CORE	####################################################################
type Cells = [Point]
type Point = (Int,Int)

-- Return the list of neighbors coordinates
getNeighbors :: Point -> [Point]
getNeighbors (x,y) = map (\(dx,dy) -> (x+dx,y+dy)) neighborsOffset

-- Return the living cells after one step 
nextStep ::  Cells -> Cells
nextStep activeList = map (\(x:xs) -> x) (filter hasToLiveFilter neighborsList)
					where neighborsList = group . sort $ concat ([ getNeighbors alive | alive <- activeList]);
								hasToLiveFilter (x:xs) =  evolve x activeList (length (x:xs))

-- Return the next state of a cell according to its neighbors
evolve :: Point -> Cells -> Int -> Bool
evolve point actives 2 | elem point actives= True
evolve _ _ 3 = True
evolve point _ _ = False

neighborsOffset = [	(-1,-1),(0,-1),(1,-1),
             				(-1, 0),       (1, 0),
             				(-1, 1),(0, 1),(1, 1)]

-- End Core ##################################################################

-- File reader ###############################################################

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

preTransformLine :: [String] -> Int -> Grid -> Grid
preTransformLine [] _ grid = grid
preTransformLine (l0:l1) size grid = preTransformLine l1 size (grid ++ [lRet] ++ plus)
	where (lRet, linesPlus) = transformLine l0 size;
		  plus = replicate linesPlus (replicate size False)

transformLine :: String -> Int -> (Row Value, Int)
transformLine l s = transformLine' l 0 (replicate s False)

--Parses the string, and modifies the line starting at the Int char
transformLine' :: String -> Int -> Row Value -> (Row Value, Int)
transformLine' [] _ r = (r,0)
transformLine' l s r
    | isJust num =
      if length (snd (fromJust num)) > 0 then
        if head (snd (fromJust num)) == 'o' then
          transformLine' (tail (snd (fromJust num))) (nb + s) newList else
          transformLine' (tail (snd (fromJust num))) (nb + s) r
        else (r,(nb-1))
    | head l == 'o' = transformLine' (tail l) (s + 1) (r !!= (s, True))
    | head l == 'b' = transformLine' (tail l) (s + 1) r
    | otherwise     =  transformLine' (tail l) s r
    where num     = parse (oneOrMore digit) l
          newList = r !!!= (s, replicate nb True)
          nb      = read (fst (fromJust num))

	

-- Changes the element at the given position by the given element in a list.
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) l (i, el) | length l <= i = error ("Index out of bounds !!!!!!!! " ++ (show (length l)) ++ ">=" ++ (show i))
				| i < 0 = error "Negative index !!"
				| otherwise = take i l ++ [el] ++ drop (i+1) l
				
-- Changes the elements at the given position by the given elements.
(!!!=) :: [a] -> (Int,[a]) -> [a]
(!!!=) l (i, el) | length l <= i = error "Index out of bounds !!"
				| i < 0 = error "Negative index !!"
				| otherwise = take i l ++ el ++ drop (i+ length el) l

--Split function
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

-- Return coordinate of living cells
getLivingsCoord :: Grid -> Cells
getLivingsCoord grid = filter (\(a,b)-> grid !! b !! a) (concat [ [ (y,x) | y <- [0..width] ] | x<-[0..heigh] ])
										where heigh = length grid -1;
													width = length (head grid) - 1
------------------------------------------------
example=[(2::Int,0::Int),(2::Int,1::Int),(2::Int,3::Int)]

