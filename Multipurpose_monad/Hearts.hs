import CardMonad

firtsPlayer :: (Num a, Eq a, Ord a) => [a] -> a
firtsPlayer = head -- Это сделано для единообразного извлечения игрока из списка

secondPlayer :: (Num a, Eq a, Ord a) => [a] -> a
secondPlayer = last . take 2

thirdPlayer :: (Num a, Eq a, Ord a) => [a] -> a
thirdPlayer = last . take 3

fourthPlayer :: (Num a, Eq a, Ord a) => [a] -> a
fourthPlayer = last -- Это тоже

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
-- (поскольку у нас 36 карт, то очков будет 22, а не 26)
shootingTheMoon :: [Int] -> Game [Int] 
shootingTheMoon xs
		| firtsPlayer xs	== 22	= P1Won
		| secondPlayer xs	== 22	= P2Won
		| thirdPlayer xs	== 22	= P3Won
		| fourthPlayer xs	== 22	= P4Won

-- Определяет победителя, если никто не "прокрутил динамо"
getWinner :: [Int] -> Game [Int]
getWinner xs
		| minimum xs == firtsPlayer xs	= P1Won
		| minimum xs == secondPlayer xs	= P2Won
		| minimum xs == thirdPlayer xs	= P3Won
		| minimum xs == fourthPlayer xs	= P4Won
