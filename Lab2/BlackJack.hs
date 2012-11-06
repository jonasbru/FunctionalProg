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
import System.Random


--Hands to use
hand1 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)--value 12
hand2 = Add (Card Ace Hearts) (Add (Card Jack Spades) Empty)        --value 21
hand3 = Add (Card Ace Hearts) (Add (Card Ace Spades) Empty)         --value 2
hand4 = Add (Card Jack Hearts) (Add (Card Jack Spades)              --value 30
	(Add (Card Jack Spades) Empty) ) 

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
winner hPlayer hBank 	| gameOver hPlayer 
							|| (not (gameOver hBank) && vhb >= vhp) = Bank
						| otherwise = Guest
	where	vhp = value hPlayer;
			vhb = value hBank


(<+) :: Hand -> Hand -> Hand
(<+) Empty h2      = h2
(<+) (Add c h1) h2 = Add c (h1 <+ h2)


fullDeck :: Hand
fullDeck = fullSuit Hearts <+ fullSuit Spades 
			<+ fullSuit Diamonds <+ fullSuit Clubs

fullSuit :: Suit -> Hand
fullSuit s = Add (Card Ace s) (Add (Card (Numeric 2) s) 
	(Add (Card (Numeric 3) s) (Add (Card (Numeric 4) s) 
	(Add (Card (Numeric 5) s) (Add (Card (Numeric 6) s) 
	(Add (Card (Numeric 7) s) (Add (Card (Numeric 8) s) 
	(Add (Card (Numeric 9) s) (Add (Card (Numeric 10) s) 
	(Add (Card Jack s) (Add (Card Queen s) 
	(Add (Card King s) Empty))))))))))))
	

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, (Add card hand))

playBank :: Hand -> Hand
playBank deck = bankHand'
	where (deck', bankHand') = playBank' deck Empty

playBank' :: Hand -> Hand -> (Hand, Hand)
playBank' deck bankHand 
	| gameOver bankHand || value bankHand >= 16 = (deck, bankHand)
	| otherwise	= playBank' deck' bankHand'
	where (deck', bankHand') = draw deck bankHand

shuffle :: StdGen -> Hand -> Hand
shuffle g hand = shuffle' g hand Empty


shuffle' :: StdGen -> Hand -> Hand -> Hand
shuffle' _ Empty hand2 = hand2
shuffle' g hand1 hand2 = shuffle' g' hand1' (Add c hand2)
	where (n, g') = randomR (0, (size hand1)-1) g;
		  (hand1', c) = getNCard hand1 n


--Gets the nth card from hand 
--and returns the card and the hand without the card
getNCard :: Hand -> Int -> (Hand, Card)
getNCard hand n = getNCard' Empty hand n 

-- removes the n card from hand2 
-- and returns ((hand1 + hand2), (n card + finalHand))
getNCard' :: Hand -> Hand -> Int -> (Hand, Card)
getNCard' _ Empty _ = error "getNCard': The deck is empty."
getNCard' hand1 (Add c hand2) n
	| n == 0 = ((hand1 <+ hand2), c)
	| otherwise = getNCard' (Add c hand1) hand2 (n-1)


--Props

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1 <+ (p2 <+ p3 ) == (p1 <+ p2 ) <+ p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf p1 p2 = size p1 + size p2 == size (p1 <+ p2) 


prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
	c `belongsTo` h == c `belongsTo` shuffle g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty	= False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g h = size h == size (shuffle (mkStdGen 10) h)

implementation = Interface
	{iEmpty	= empty
	, iFullDeck = fullDeck
	, iValue	= value
	, iGameOver = gameOver
	, iWinner	= winner
	, iDraw	= draw
	, iPlayBank = playBank
	, iShuffle	= shuffle
	}

main :: IO()
main = runGame implementation

































