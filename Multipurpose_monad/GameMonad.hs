module GameMonad where
import Control.Monad

data Game a =  PnWon Int | Scoring a
				deriving (Show, Eq, Ord, Read)
				
instance Functor (Game n) where
	fmap f (Scoring a) 	= Scoring (f a)
	fmap f (PnWon n)	= PnWon n
	
instance Monad (Game n) where
	return  			= Scoring 
	Scoring a 	>>= f	= f a
	PnWon n		>>= _ 	= PnWon n

applyAllChecks ::  (Monad m) => m a -> [(a -> m a)] -> m a
y `applyAllChecks` xs = foldl (>>=) y xs --(foldr (.) id $ map (=<<) xs) y

