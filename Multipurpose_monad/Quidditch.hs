module Quidditch where
import GameMonad

data Ball = Snitch | Quaffle deriving (Eq, Show, Read)

score :: [Ball] -> Int
score xs
		| Snitch `elem` xs	= 150 + ((length xs) - 1) * 10
		| otherwise 		= (length xs) * 10

fastEndOfGame :: ([Ball],[Ball]) -> Game Int ([Ball],[Ball])
fastEndOfGame (xs, ys)
		| Snitch `elem` xs	&& ys == []	= PnWon 1
		| Snitch `elem` ys	&& xs == []	= PnWon 2
		| otherwise						= return (xs, ys)

standartEndOfGame :: ([Ball],[Ball]) -> Game Int ([Ball],[Ball])
standartEndOfGame (xs, ys)
		| Snitch `elem` xs && (length ys) * 10 <= 150	= PnWon 1
		| Snitch `elem` ys && (length xs) * 10 <= 150	= PnWon 2
		| otherwise										= return (xs, ys)

rareCase :: ([Ball],[Ball]) -> Game Int ([Ball],[Ball])
rareCase (xs, ys)
		| score xs > score ys = PnWon 1
		| score xs < score ys = PnWon 2

decideWinner :: Game Int ([Ball],[Ball]) -> Game Int ([Ball],[Ball])
decideWinner a = a `applyAllChecks` [fastEndOfGame, standartEndOfGame,
									rareCase]
