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

empty :: Hand
empty = Empty


value :: Hand → Integer
value Empty = 0


valueRank :: Rank -> Integer
valueRank (Numeric x) = x 
valueRank Ace	= 11
valueRank otherwise = 10

valueCard :: Card -> Integer


numberOfAces :: Hand -> Integer
numberOfAces Empty  = 0
numberOfAces (Add card hand)
	| rank card == 11 = 1 + numberOfAces(Hand)
	| otherwise = numberOfAces(Hand)

