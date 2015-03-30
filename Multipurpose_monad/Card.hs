module Card where
import GameMonad
import Control.Applicative

data Suit = C | D | H | S deriving (Show, Read, Eq)

data Value = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | J | Q | K | A 
	deriving (Ord, Eq, Read, Show, Enum)

data Card = Card {
		  getValue :: Value 
		, getSuit  :: Suit
		} deriving (Show)

instance Ord Card where
	(Card v1 _) `compare`  (Card v2 _) = v1 `compare` v2 

instance Eq Card where
	(Card v1 _) ==  (Card v2 _) = v1 == v2
