{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Quidditch where
import GameClass
import GameMonad

data Ball = Snitch | Quaffle deriving (Eq, Show, Read)

newtype EndOfGame = EndOfGame {getListOfBalls :: ([Ball],[Ball])}

-- Превращает строку в список партий (EndOfGame'ов)
stringToListOfGames :: [String] -> [EndOfGame]
stringToListOfGames = map EndOfGame
						. map (\xs -> (take 2 xs, drop 2 xs)) 
						. map (map read) 
						. map words

toMonad :: [EndOfGame] -> [Game Quidditch EndOfGame]
toMonad = map return

-- Подсчёт очков команды
score :: [Ball] -> Int
score xs
		| Snitch `elem` xs	= 150 + ((length xs) - 1) * 10
		| otherwise 		= (length xs) * 10

-- Быстрый конец игры: снитч был пойман в самом начале одной
-- из команд
fastEndOfGame :: EndOfGame -> Game Quidditch EndOfGame
fastEndOfGame a = check $ getListOfBalls a
		where
			check (xs, ys)
				| Snitch `elem` xs	&& ys == []	= Player 1
				| Snitch `elem` ys	&& xs == []	= Player 2
				| otherwise						= return a

-- Обычный конец игры: прежде чем снитч был пойман обе команды
-- успели набрать до 150 очков
standartEndOfGame :: EndOfGame -> Game Quidditch EndOfGame
standartEndOfGame a = check $ getListOfBalls a
		where
			check (xs, ys)
				| Snitch `elem` xs && (length ys) * 10 <= 150
														= Player 1
				| Snitch `elem` ys && (length xs) * 10 <= 150
														= Player 2
				| otherwise								= return a

-- Редкий случай: команда, поймавшая снитч, всё равно проиграла,
-- поскольку у вражеской команды много очков.
rareCase :: EndOfGame -> Game Quidditch EndOfGame
rareCase a = check $ getListOfBalls a
		where
			check (xs, ys)
				| score xs > score ys = Player 1
				| score xs < score ys = Player 2

data Quidditch

instance PlayGame (Game Quidditch) where
	type GameInfo (Game Quidditch)	= EndOfGame
	getPlayer (Player n)				= n
	getGameInfo (Scoring a)			= a
	checks							= [fastEndOfGame, standartEndOfGame,
									rareCase]
