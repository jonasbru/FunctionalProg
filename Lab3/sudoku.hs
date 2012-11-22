-- Functional Programming -- Lab Assignment 3A
-- Michael Fagno && Jonas Bru
-- The file was run through hlint without warnings.

module Sudoku where

import Test.QuickCheck
import Data.Char
import Data.List
import System.Exit
import Data.Maybe
import Debug.Trace

--A-----------------------------------------------------------------------

data Sudoku = Sudoku { rows :: [[Maybe Int]] }
 deriving ( Show, Eq )

-- allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [ [ Nothing | i <- [1..9] ]  | j <- [1..9] ]

-- isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku sudok = 
	length (rows sudok) == 9 
		&& all (\row -> length row == 9) (rows sudok) --check the length of each row
		&& all isValid (concat (rows sudok)) -- check values

-- Check if the content of a cell is valid (integer between 1 and 9 or Nothing)
isValid :: Maybe Int -> Bool									
isValid (Just a) = a < 10 && a > 0
isValid Nothing = True

-- isSolved sud checks if sud is already solved, i.e. there are no blanks
isSolved :: Sudoku -> Bool
isSolved s = all (/= Nothing) (concat (rows s))

--B-----------------------------------------------------------------------

-- printSudoku sud prints a representation of the sudoku sud on the screen
printSudoku :: Sudoku -> IO ()
printSudoku xs =
	mapM_ putStrLn lines
	where lines = [listToString line
			|line <- rows xs ]

-- Convert a list of Maybe Int in a single String
listToString :: [Maybe Int] -> String
listToString list = concat [ b | b <- map toString list]

-- Convert a Maybe Int in a string
toString :: Maybe Int -> String
toString (Just a) = show a
toString Nothing = "."

-- readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku file = do 
	g <- readFile file
	let s = Sudoku [map transformLine p | p <- lines g]
	if isSudoku s && isOkay s
		then return (Sudoku [map transformLine p | p <- lines g])
		else er 
	where er = error "Sudoku not valid !!" 

-- Returns the case corresponding to the char
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
prop_Sudoku = isSudoku

--D-----------------------------------------------------------------------

type Block = [Maybe Int]

-- Returns true if a block doesn't contain twice the same digit
isOkayBlock :: Block -> Bool
isOkayBlock b = length b == length (nubBy eqOnlyInt b)
	where eqOnlyInt c1 c2 = c1 /= Nothing && c2 /= Nothing && c1 == c2

-- Returns all the blocks from a sudoku (27 blocks)
blocks :: Sudoku -> [Block]
blocks s = rows s ++ blocksCols s ++ blocksSquares s

-- Returns all the 9 columns of the sudoku
blocksCols :: Sudoku -> [Block]
blocksCols s = [ [ (rows s !! i) !! j | j <- [0..8] ] | i <- [0..8] ]

-- Returns all the 3x3 blocks
blocksSquares :: Sudoku -> [Block]
blocksSquares s = 
	[ [ (rows s !! (i*3 + k)) !! (j*3 + l) | k <- [0..2], l <- [0..2] ] 
		| i <- [0..2], j <- [0..2] ]

-- Checks that there are 27 blocks of 9 cases each
prop_sizeBlocks :: Sudoku -> Bool
prop_sizeBlocks s = 
	length b == 3*9
	&& all (\v -> length v == 9) b
	where b = blocks s

-- True if all the blocks of the sudoku don't contain twice the same digit
isOkay :: Sudoku -> Bool
isOkay s = all isOkayBlock (blocks s)

--E-----------------------------------------------------------------------

type Pos = (Int,Int)

-- Return the list of Positions where there are blanks
blanks :: Sudoku -> [Pos]
blanks sudok = 
	filter (\(r,c)->rows sudok!!(r)!!(c) == Nothing) [(i,j) | i<-[0..8], j<-[0..8]]

prop_blanks :: Sudoku -> Bool
prop_blanks sudok = and (map (\(r,c)->rows sudok!!(r)!!(c) == Nothing) (blanks sudok))

-- Changes the element at the given position by the given element in a list.
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) l (i, el) | length l <= i = error "Index out of bounds !!"
				| i < 0 = error "Negative index !!"
				| otherwise = take (i) l ++ [el] ++ drop (i+1) l

prop_insert :: [Int] -> (Int, Int) -> Bool
prop_insert l (i, el) = ((l !!= (i, el)) !! i) == el

-- Replaces an element in a sudoku
update :: Sudoku -> Pos -> Maybe Int -> Sudoku
update s (r, c) v = Sudoku (rows s !!= (r, ((rows s !! r) !!=  (c, v))))

prop_update :: Sudoku -> Pos -> Maybe Int -> Bool 
prop_update s (r, c) v = rows s' !! r !! c == v
	where s' = update s (r,c) v

-- Returns all the valid numbers of a Pos
candidates :: Sudoku -> Pos -> [Int]
candidates s (r,c) 
	| rows s !! r !! c /= Nothing = error "Case not empty !!"
	| otherwise = [1..9] \\ (catMaybes (gimmeMyNumbers s (r,c)))

-- Given a position, returns the 3 blocks of the position
gimmeMyBlocks :: Sudoku -> Pos -> [Block]
gimmeMyBlocks s (r,c) = [(rows s !! r)] ++
	 [[ (rows s !! i) !! c | i <- [0..8] ]] ++
	 [[ (rows s !! ((c `div` 3) * 3 + k)) !! ((r `div` 3) * 3 + l) | k <- [0..2], l <- [0..2] ]]
	 
-- Given a position, returns the different numbers on the blocks from the pos
gimmeMyNumbers :: Sudoku -> Pos -> Block
gimmeMyNumbers s (r,c) = (rows s !! r) `union`
	 [ (rows s !! i) !! c | i <- [0..8] ] `union`
	 [ (rows s !! ((c `div` 3) * 3 + k)) !! ((r `div` 3) * 3 + l) | k <- [0..2], l <- [0..2] ]
	 
-- Tests that the candidates are valid
prop_candidates :: Sudoku -> Pos -> Bool
prop_candidates s p = all testSudoku (candidates s p)
	where testSudoku = \i -> isSudoku (update s p (Just i)) 
							&& isOkay (update s p (Just i));

--F-----------------------------------------------------------------------

solve :: Sudoku -> Maybe Sudoku
solve sud = if isSudoku sud && isOkay sud 						
								then solve' sud
								else Nothing

solve' :: Sudoku -> Maybe Sudoku
solve' sud = case blanks sud of
							[] -> trace ("s':No More blank ") Just sud
							pos:q -> trace ("s':blank " ++ show pos ++ "cases left: " ++ show (length (pos:q))) testCandidates (candidates sud pos) sud pos
							
testCandidates :: [Int] -> Sudoku -> Pos -> Maybe Sudoku
testCandidates (candidate:otherCand) sud pos=	
	trace ("tc:testCandidates " ++ show  pos ++ " Values: " ++ show (candidate:otherCand)) 
	(case solve' (update sud pos (Just candidate)) of
		Nothing -> trace ("tc:Bad Candidate " ++ show  pos ++ " Value: " ++ show candidate) testCandidates otherCand sud pos-- Try an other candidate
		sudokuSolved ->	trace "tc:Found" sudokuSolved )-- Done										
testCandidates [] _ _ = trace ("tc:NOTHING" ) Nothing

-- Reads, solves, and prints a sudoku
readAndSolve :: FilePath -> IO ()
readAndSolve file = do 
	s <- readSudoku file  
	printSudoku (fromJust (solve s))


isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf = undefined


prop_SolveSound :: Sudoku -> Property
prop_SolveSound = undefined

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
{-
  364871295
  752936184
  819254736
  596713428
  431582679
  278469351
  645328917
  983147562
  127695843
-}
example2 =
    Sudoku
      [ [Just 1,Just 2,Just 3,Nothing,Just 5,Just 6,Just 7,Nothing,Nothing]
      , [Just 4,Just 5,Just 6,Just 7,Just 8,Just 9,Just 1,Just 2,Just 3]
      , [Just 7,Just 8,Just 9,Just 1,Just 2,Just 3,Just 4,Just 5,Just 6]
      , [Just 2,Just 1,Just 4,Just 3,Just 6,Just 5,Just 8,Just 9,Just 7]
      , [Just 3,Just 6,Just 5,Just 8,Just 9,Just 7,Just 2,Just 1,Just 4]
      , [Just 8,Just 9,Just 7,Just 2,Just 1,Just 4,Just 3,Just 6,Just 5]
      , [Just 5,Just 3,Just 1,Just 6,Just 4,Just 2,Just 9,Just 7,Just 8]
      , [Just 6,Just 4,Just 2,Just 9,Just 7,Just 8,Just 5,Just 3,Just 1]
      , [Just 9,Just 7,Just 8,Just 5,Just 3,Just 1,Just 6,Just 4,Just 2]
      ]
example3 =
    Sudoku
      [ [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
      , [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Just 1]
      , [Just 9,Just 7,Just 8,Just 5,Just 3,Just 1,Just 6,Just 4,Just 2]
      ]
{-
123456789
456789123
789123456
214365897
365897214
897214365
531642978
642978531
978531642
-}

