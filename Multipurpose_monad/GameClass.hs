{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-} --, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, UndecidableInstances, TupleSections #-}

module GameClass where

foldM :: (Monad m) => m a -> [(a -> m a)] -> m a
y `foldM` xs = foldl (>>=) y xs

class (Functor g, Monad g) => PlayGame g where 
	type Payer g 
	type Payer g		= Int
	type GameInfo
	checks				:: [GameInfo -> g GameInfo]
	getPlayer			:: g GameInfo -> Payer g
	getGameInfo			:: g GameInfo -> GameInfo
	
	performGame			:: g GameInfo -> g GameInfo
	performGame	p		= p `foldM` checks
	
	decideWinner		:: g GameInfo -> Payer g
	decideWinner		= getPlayer . performGame
