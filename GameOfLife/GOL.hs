module GOL where

import Data.List
import Test.QuickCheck
import Debug.Trace
import System.Console.ANSI

type Grid = Matrix Value
type Matrix a = [Row a]
type Row a = [a]
type Value = Bool


rows :: Matrix a -> [Row a]
rows = id


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
