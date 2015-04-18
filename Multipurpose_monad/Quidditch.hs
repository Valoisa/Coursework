{-# LANGUAGE TypeFamilies #-}
module Quidditch where
import GameClass
import GameMonad

data Ball = Snitch | Quaffle deriving (Eq, Show, Read)

newtype EndOfGame = EndOfGame {getListOfBalls :: ([Ball],[Ball])}

-- Подсчёт очков команды
score :: [Ball] -> Int
score xs
		| Snitch `elem` xs	= 150 + ((length xs) - 1) * 10
		| otherwise 		= (length xs) * 10

-- Быстрый конец игры: снитч был пойман в самом начале одной
-- из команд
fastEndOfGame :: EndOfGame -> Game EndOfGame
fastEndOfGame a = check $ getListOfBalls a
		where
			check (xs, ys)
				| Snitch `elem` xs	&& ys == []	= PnWon 1
				| Snitch `elem` ys	&& xs == []	= PnWon 2
				| otherwise						= return a

-- Обычный конец игры: прежде чем снитч был пойман обе команды
-- успели набрать до 150 очков
standartEndOfGame :: EndOfGame -> Game EndOfGame
standartEndOfGame a = check $ getListOfBalls a
		where
			check (xs, ys)
				| Snitch `elem` xs && (length ys) * 10 <= 150
														= PnWon 1
				| Snitch `elem` ys && (length xs) * 10 <= 150
														= PnWon 2
				| otherwise								= return a

-- Редкий случай: команда, поймавшая снитч, всё равно проиграла,
-- поскольку у вражеской команды много очков.
rareCase :: EndOfGame -> Game EndOfGame
rareCase a = check $ getListOfBalls a
		where
			check (xs, ys)
				| score xs > score ys = PnWon 1
				| score xs < score ys = PnWon 2

instance PlayGame Game where
	type GameInfo			= EndOfGame
	getPlayer (PnWon n)	= n
	getGameInfo (Scoring a)	= a
	checks					= [fastEndOfGame, standartEndOfGame,
									rareCase]
