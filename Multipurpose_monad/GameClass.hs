{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-} --, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, TupleSections #-}

module GameClass where

foldM :: (Monad m) => m a -> [(a -> m a)] -> m a
y `foldM` xs = foldl (>>=) y xs

class (Functor g, Monad g) => PlayGame g where 
	type Payer g 
	type Payer g		= Int
	type GameInfo g
	checks				:: [GameInfo g -> g (GameInfo g)]
	getPlayer			:: g (GameInfo g) -> Payer g
	getGameInfo			:: g (GameInfo g) -> (GameInfo g)


performGame			:: (PlayGame g) => g (GameInfo g) -> g (GameInfo g)
performGame	p		= p `foldM` checks

decideWinner		:: (PlayGame g) => g (GameInfo g) -> Payer g
decideWinner		= getPlayer . performGame
