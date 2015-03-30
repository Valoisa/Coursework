module CardMonad where
import Data.List
import Control.Monad
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

data Game a = Scoring a | P1Won | P2Won | P3Won | P4Won
				deriving (Show, Eq, Ord, Read)
instance Functor Game where
	fmap f (Scoring a) 		= Scoring (f a)
	fmap f P1Won			= P1Won
	fmap f P2Won			= P2Won
	fmap f P3Won			= P3Won
	fmap f P4Won			= P4Won
	
instance Monad Game where
	return x 		= Scoring x
	Scoring x 	>>= f	= f x
	_ 			>>= f 	= _

applyAllChecks ::  m a -> [(a -> m a)] -> m a
y `applyAllChecks` xs = (foldr (.) id $ map (=<<) xs) y

decideTheWinner :: Game a -> [a -> Game b] -> Game b
decideTheWinner = undefined
