module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List

--A-----------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [ [ Nothing | i <- [1..9] ]  | j <- [1..9] ]

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sudok = length (rows sudok) == 9 
									&& all (\row -> length row == 9) (rows sudok) --check the length of each row
									&& all isValid (concat (rows sudok)) -- check values
									
isValid (Just a) = (a < 10 && a > 0)
isValid Nothing = True

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved s = all (\v -> v /= Nothing) (concat (rows s))

--B-----------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku xs =
	sequence_ (map putStrLn lines)
	where lines = [listToString line
			|line <- (rows xs) ]

listToString list = foldr (++) [] [ b | b <- map toString list]

toString (Just a) = show a
toString Nothing = "."

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do 
	g <- readFile file
	return (Sudoku [map transformLine p | p <- lines g])
	
transformLine :: Char -> Maybe Int
transformLine '.' = Nothing
transformLine c = Just (digitToInt c)

--C-----------------------------------------------------------------------

-- cell generates an arbitrary cell in a Sudoku
cell :: Gen (Maybe Int)
cell = frequency
				[(9, return Nothing),
				(1, do 
							r<-(choose(1,9) :: Gen Int) 
							return (Just r))]


-- an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary =
    do rows <- sequence [ sequence [ cell | j <- [1..9] ] | i <- [1..9] ]
       return (Sudoku rows)
       
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku s = isSudoku s

--D-----------------------------------------------------------------------

type Block = [Maybe Int]

-- Returns true if a block doesn't contain twice the same digit
isOkayBlock :: Block -> Bool
isOkayBlock b = length b == length (nubBy eqOnlyInt b)
	where eqOnlyInt c1 c2 = c1 /= Nothing && c2 /= Nothing && c1 == c2

-- Returns all the blocks from a sudoku (27 blocks)
blocks :: Sudoku -> [Block]
blocks s = (rows s) ++ (blocksCols s) ++ (blocksSquares s)

-- Returns all the 9 columns of the sudoku
blocksCols :: Sudoku -> [Block]
blocksCols s = [ [ ((rows s) !! i) !! j | j <- [0..8] ] | i <- [0..8] ]

-- Returns all the 3x3 blocks
blocksSquares :: Sudoku -> [Block]
blocksSquares s = [ [ ((rows s) !! (i*3 + k)) !! (j*3 + l) | k <- [0..2], l <- [0..2] ] | i <- [0..2], j <- [0..2] ]

-- Checks that there are 27 blocks of 9 cases each
prop_sizeBlocks :: Sudoku -> Bool
prop_sizeBlocks s = 
	length b == 3*9
	&& and (map (\v -> length v == 9) b)
	where b = blocks s

isOkay :: Sudoku -> Bool
isOkay s = and (map (\b -> isOkayBlock b) (blocks s))

-------------------------------------------------------------------------

-- TESTS
example :: Sudoku
example =
    Sudoku
      [ [Just 3, Just 6, Nothing,Nothing,Just 7, Just 1, Just 2, Nothing,Nothing]
      , [Nothing,Just 5, Nothing,Nothing,Nothing,Nothing,Just 1, Just 8, Nothing]
      , [Nothing,Nothing,Just 9, Just 2, Nothing,Just 4, Just 7, Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Just 1, Just 3, Nothing,Just 2, Just 8]
      , [Just 4, Nothing,Nothing,Just 5, Nothing,Just 2, Nothing,Nothing,Just 9]
      , [Just 2, Just 7, Nothing,Just 4, Just 6, Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Just 5, Just 3, Nothing,Just 8, Just 9, Nothing,Nothing]
      , [Nothing,Just 8, Just 3, Nothing,Nothing,Nothing,Nothing,Just 6, Nothing]
      , [Nothing,Nothing,Just 7, Just 6, Just 9, Nothing,Nothing,Just 4, Just 3]
      ]
