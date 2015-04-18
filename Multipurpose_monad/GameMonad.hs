module GameMonad where
import GameClass
import Control.Monad

data Game a = PnWon Int | Scoring a
				deriving (Show, Eq, Ord, Read)

instance Functor Game where
	fmap f (Scoring a) 	= Scoring (f a)
	fmap f (PnWon n)	= PnWon n

instance Monad Game where
	return  			= Scoring 
	Scoring a 	>>= f	= f a
	PnWon n		>>= _ 	= PnWon n
