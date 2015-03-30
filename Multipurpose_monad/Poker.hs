module Poker where
import Card
import GameMonad
import Data.List
import Data.Ord (comparing)
import Control.Monad

data HandType = HighCard | OnePair | TwoPairs |
			ThreeOfAKind | Straight | Flush |
			FullHouse | FourOfAKind | StraightFlush | RoyalFlush
			deriving (Ord, Eq, Show)

type Hand = (HandType, [Card])

{- ******* Вспомогательные функции ******* -}
-- Превращает список карт из файла в список десяток
makeTens :: [a] -> [[a]]
makeTens xs = map (take 10) 
	$ takeWhile (not . null) $ iterate (drop 10) xs

-- Чтение одной карты
readCard :: String -> Card
readCard xs = Card (readValue $ head xs) (readSuit $ last xs)
    where
        readValue a
            | a == '2'  = C2
            | a == '3'  = C3
            | a == '4'  = C4
            | a == '5'  = C5
            | a == '6'  = C6
            | a == '7'  = C7
            | a == '8'  = C8
            | a == '9'  = C9
            | a == 'T'  = C10
            | otherwise = (read::String -> Value) [a]
        readSuit b = (read::String -> Suit) [b]

-- Превращает список десяток в список пар, 
-- элементы которых - пятёрки	(ставки)
makePairs :: (Ord a) => [[a]] -> [([a], [a])]
makePairs ys = map (\xs -> (take 5 xs, drop 5 xs)) ys

--Чтение списка карт
readCardList :: [String] -> [([Card], [Card])]
readCardList  = makePairs . makeTens . map readCard

putToMonad :: [([Card], [Card])] -> [Game Int ([Card], [Card])]
putToMonad = map return

--Составление пары ставок
makeHand :: ([Card], [Card]) -> Game Int (Hand, Hand)
makeHand (a, b) = return (identHand a, identHand b)

-- Список достоинств
valList :: [Card] -> [Value]
valList = map getValue

--Другие вспомогательные функции
sortByLength = sortBy (comparing length)

sortGroups = sortByLength . group

localHighestCard =  head . last . sortGroups

getHighFromTwoPairs = maximum . drop 1 . concat . sortGroups

{- ******* Определение типа ставки: ******* -}
-- Флэш 
isFlush :: [Card] -> Bool
isFlush = (== 1) . length . nub . map getSuit

-- Стрит
isStraight:: [Card] -> Bool
isStraight xs = vals `elem`
	[take 5 [head vals..], [C2, C3,C4, C5, A]]
	where
		vals = valList xs

-- Стрит флэш		
isStraightFlush :: [Card] -> Bool
isStraightFlush xs = isFlush xs && isStraight xs

-- Флэш рояль
isRoyalFlush :: [Card] -> Bool
isRoyalFlush xs = isFlush xs && 
				map getValue xs == [C10 .. A]

-- Каре
isFourOfAKind :: [Card] -> Bool
isFourOfAKind = (== [1, 4]) . sort . groupsLength 

-- Уникальные достоинства в списке карт
uniqueVals = nub . valList

-- Фулл хаус
isFullHouse :: [Card] -> Bool
isFullHouse = (== 2) . length . uniqueVals

-- Одна пара
isOnePair :: [Card] -> Bool
isOnePair = (== 4) . length . uniqueVals

-- Две пары
isTwoPairs :: [Card] -> Bool
isTwoPairs = (== [1, 2, 2]) . sort . groupsLength 

-- Тройка
isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind = (3 `elem`) . groupsLength

-- Рамеры групп при группировке по достоинству
groupsLength = map length . group . valList

-- Возвращает старшую карту
highestCard :: [Card] -> Card
highestCard = maximum

{-****Функции для проверки на выигрыш****-}
identHand :: [Card] -> Hand
identHand = identSortedHand . sort

identSortedHand :: [Card] -> Hand
identSortedHand xs 
	| isRoyalFlush xs		= (RoyalFlush, xs)
	| isStraightFlush xs	= (StraightFlush, xs)
	| isFlush xs			= (Flush, xs)
	| isStraight xs			= (Straight, xs)
	| isFourOfAKind xs		= (FourOfAKind, xs)
	| isFullHouse xs		= (FullHouse, xs)
	| isThreeOfAKind xs		= (ThreeOfAKind, xs)
	| isTwoPairs xs 		= (TwoPairs, xs)
	| isOnePair xs 			= (OnePair, xs)
	| otherwise 			= (HighCard, xs)

{- *****Монадические функции***** -}
-- compareEqualHandType разбиваем на три функции
firstDecideTheWinner xs ys 	= if highestCard xs > highestCard ys then
								PnWon 1 else PnWon 2

secondDecideTheWinner xs ys = 
						if localHighestCard xs > localHighestCard ys
						then PnWon 1 else PnWon 2

firstCheck :: (Hand, Hand) -> Game Int (Hand, Hand)
firstCheck hand@((a1, xs), (a2, ys)) 
	| a1 `elem` [HighCard, Flush, Straight, StraightFlush]
				= firstDecideTheWinner xs ys
	| otherwise = return hand

secondCheck :: (Hand, Hand) -> Game Int (Hand, Hand)
secondCheck hand@((a1, xs), (a2, ys))
	| a1 `elem` [OnePair, ThreeOfAKind, FourOfAKind]
				= secondDecideTheWinner xs ys
	| otherwise = return hand

thirdCheck :: (Hand, Hand) -> Game Int (Hand, Hand)
thirdCheck hand@((a1, xs), (a2, ys))
	| a1 == FullHouse 	= compareFullHouse xs ys
	| otherwise 		= return hand

compareFullHouse xs ys = 
	case compare (localHighestCard xs) (localHighestCard ys) of
		GT	-> PnWon 1
		LT	-> PnWon 2
		_	-> if getHighInPair xs > getHighInPair ys then PnWon 1
				else PnWon 2
	where
		getHighInPair = head . head . sortGroups

--compareHand :: Game PokerHand -> Game PokerHand
--compareHand :: (PokerHand, PokerHand) -> Game PokerHand
compareHand :: (Hand, Hand) -> Game Int (Hand, Hand)
compareHand (h1@(a1, xs), h2@(a2, ys))
	| a1 < a2 			= PnWon 2
	| a1 > a2 			= PnWon 1
	| a1 == RoyalFlush	= PnWon 2
	| otherwise 		= return (h1, h2)

decideWinner :: Game Int (Hand, Hand) -> Game Int (Hand, Hand)
decideWinner a = a `applyAllChecks` [compareHand, firstCheck
									, secondCheck, thirdCheck]
