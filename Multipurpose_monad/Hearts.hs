{-# LANGUAGE TypeFamilies #-}

module Hearts where
import Card
import GameClass
import GameMonad

type Scores = [Int]

playerN :: Int -> Scores -> Int
playerN n = last . take n

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
shootingTheMoon :: Scores -> Game Scores
shootingTheMoon xs
		| playerN 1 xs	== 22	= PnWon 1
		| playerN 2 xs	== 22	= PnWon 2
		| playerN 3 xs	== 22	= PnWon 3
		| playerN 4 xs	== 22	= PnWon 4

-- Определяет победителя, если никто не "прокрутил динамо"
getWinner :: Scores -> Game Scores 
getWinner xs
		| minimum xs == playerN 1 xs	= PnWon 1
		| minimum xs == playerN 2 xs	= PnWon 2
		| minimum xs == playerN 3 xs	= PnWon 3
		| minimum xs == playerN 4 xs	= PnWon 4

instance PlayGame Game where
	type GameInfo			= Scores
	getPlayer (PnWon n)		= n
	getGameInfo (Scoring a)	= a
	checks					= [shootingTheMoon, getWinner]

{-decideWinner :: Game Int GameInfo -> Game Int GameInfo
decideWinner a = a `applyAllChecks` [shootingTheMoon, getWinner]-}
