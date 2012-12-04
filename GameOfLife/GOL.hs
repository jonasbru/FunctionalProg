{- 

You will need to run the followings commands : 
$> cabal update 
$> cabal install ansi-terminal

-}

module GOL where

import Data.List
import Test.QuickCheck
import Debug.Trace
import System.Console.ANSI
import Data.Maybe
import Parsing

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
toString True = 'O'
toString False = ' '

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


-- parse (oneOrMore digit) "234oiu"
-- parse (oneOrMore (char 'o')) "234oiu"


--Split function
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'



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


