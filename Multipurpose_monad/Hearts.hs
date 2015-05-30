{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

module Hearts where
import Card
import GameClass
import GameMonad
import Data.List

type Scores = [Int]

playerN :: Int -> Scores -> Int
playerN n = last . take n

-- Превращает строку в список взяток
stringToListOfBribes :: [String] -> [Scores]
stringToListOfBribes = 	 map (map score)
						. map (take 4) . takeWhile (\xs -> xs /= [])
						. iterate (drop 4). map (map readCard) 
						. map words

placeToMonad :: [Scores] -> [Game Hearts Scores]
placeToMonad = map return

-- Определяет, сколько очков приносит карта
cardToPoint :: Card -> Int
cardToPoint (Card v s)
		| s == H			= 1
		| v == Q && s == S 	= 13
		| otherwise			= 0

-- Определяет, сколько очков на руках у игрока
score :: [Card] -> Int
score = sum . map cardToPoint

-- Проверяет, что кто-то из игроков "прокрутил динамо" 
-- ("выстрелил в Луну")
-- (поскольку у нас 36 карт, то очков будет 22, а не 26)
shootingTheMoon :: Scores -> Game Hearts Scores
shootingTheMoon xs
		| playerN 1 xs	== 22	= Player 1
		| playerN 2 xs	== 22	= Player 2
		| playerN 3 xs	== 22	= Player 3
		| playerN 4 xs	== 22	= Player 4
		| otherwise				= return xs

-- Определяет победителя, если никто не "прокрутил динамо"
getWinner :: Scores -> Game Hearts Scores 
getWinner xs
		| minimum xs == playerN 1 xs	= Player 1
		| minimum xs == playerN 2 xs	= Player 2
		| minimum xs == playerN 3 xs	= Player 3
		| minimum xs == playerN 4 xs	= Player 4
		| otherwise						= return xs

--Определим экземпляр класса PlayGame
data Hearts

instance PlayGame (Game Hearts) where
	type GameInfo (Game Hearts)	= Scores
	getPlayer (Player n)			= n
	getGameInfo (Scoring a)		= a
	checks						= [shootingTheMoon, getWinner]
