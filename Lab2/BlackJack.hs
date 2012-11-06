-- Functional Programming -- Lab Assignment 2A 
-- Michael Fagno && Jonas Bru
-- The file was run through hlint without warnings.

{-
 3.2
	size hand2	
							=	size (Add (Card (Numeric 2) Hearts)
								(Add (Card Jack Spades) Empty))

							= 1 + size (Add (Card Jack Spades) Empty)
							= 1 + 1 + size(Empty)
							= 1 + 1 + 0
							= 2
-}

module BlackJack where
import Cards
import Wrapper

--Hands to use
hand1 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty) --value 12
hand2 = Add (Card Ace Hearts) (Add (Card Jack Spades) Empty)         --value 21
hand3 = Add (Card Ace Hearts) (Add (Card Ace Spades) Empty)          --value 2
hand4 = Add (Card Jack Hearts) (Add (Card Jack Spades) 
	(Add (Card Jack Spades) Empty) ) --value 30

--Functions
empty :: Hand
empty = Empty


value :: Hand -> Integer
value h | rv <= 21 || na == 0 = rv
		| otherwise = rv - (na * 10) 
	where rv = rawValue h;
		  na = numberOfAces h

rawValue :: Hand -> Integer
rawValue Empty = 0
rawValue (Add c h) = valueCard c + rawValue h


valueRank :: Rank -> Integer
valueRank (Numeric x) = x 
valueRank Ace	= 11
valueRank otherwise = 10


valueCard :: Card -> Integer
valueCard c = valueRank (rank c)


numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add card hand)
	| rank card == Ace = 1 + numberOfAces hand
	| otherwise = numberOfAces hand


gameOver :: Hand -> Bool
gameOver hand =	value hand > 21


winner :: Hand -> Hand -> Player
winner hPlayer hBank 	| gameOver hPlayer || (not (gameOver hBank) && vhb >= vhp) = Bank
						| otherwise = Guest
	where	vhp = value hPlayer;
			vhb = value hBank






























