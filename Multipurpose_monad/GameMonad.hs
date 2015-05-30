module GameMonad where
import Control.Monad

data Game g a = Player Int | Scoring a
				deriving (Show, Eq, Ord, Read)

instance Functor (Game g) where
	fmap f (Scoring a) 	= Scoring (f a)
	fmap f (Player n)	= Player n

instance Monad (Game g) where
	return  			= Scoring 
	Scoring a 	>>= f	= f a
	Player n	>>= _ 	= Player n
