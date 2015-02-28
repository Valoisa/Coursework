module Poker where
import Data.List
import Data.Ord (comparing)

data Suit = C | D | H | S deriving (Show, Read, Eq)

data Value = C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | C10 | J | Q | K | A 
	deriving (Ord, Eq, Read, Show, Enum)

data Card = Card {
		  getValue :: Value 
		, getSuit  :: Suit
		} deriving (Show)

instance Ord Card where
	(Card v1 _) `compare`  (Card v2 _) = v1 `compare` v2 

instance Eq Card where
	(Card v1 _) ==  (Card v2 _) = v1 == v2 

data HandType = HighCard | OnePair | TwoPairs |
			ThreeOfAKind | Straight | Flush |
			FullHouse | FourOfAKind | StraightFlush | RoyalFlush
			deriving (Ord, Eq, Show)

type Hand = (HandType, [Card])

type Game = (Hand, Hand)

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

--Чтение списка карт
readCardList :: [String] -> [([Card], [Card])]
readCardList  = makePairs . makeTens . map readCard

--Составление пары ставок
makeHand :: ([Card], [Card]) -> (Hand, Hand)
makeHand (a, b) = (identHand a, identHand b)

-- Превращает список десяток в список пар, 
-- элементы которых - пятёрки	(ставки)
makePairs :: (Ord a) => [[a]] -> [([a], [a])]
makePairs ys = map (\xs -> (take 5 xs, drop 5 xs)) ys

-- Список достоинств
valList :: [Card] -> [Value]
valList = map getValue

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
	| isStraightFlush xs		= (StraightFlush, xs)
	| isFlush xs				= (Flush, xs)
	| isStraight xs			= (Straight, xs)
	| isFourOfAKind xs		= (FourOfAKind, xs)
	| isFullHouse xs			= (FullHouse, xs)
	| isThreeOfAKind xs		= (ThreeOfAKind, xs)
	| isTwoPairs xs 			= (TwoPairs, xs)
	| isOnePair xs 			= (OnePair, xs)
	| otherwise 					= (HighCard, xs)

-- Если у игроков одинаковые ставки	
compareEqualHandType :: (Hand, Hand) -> Bool
compareEqualHandType (h1@(a1, _), h2@(a2, _)) = 
	if a1 /= a2 then
		error "compareAlikeHand get unequal hand types" else
		compareEqualHandType' (h1, h2)

compareEqualHandType' :: (Hand, Hand) -> Bool
compareEqualHandType' ((a1, xs), (a2, ys))
	| a1 `elem` [HighCard, Flush, Straight, StraightFlush] 
			= highestCard xs > highestCard ys
	| a1 `elem` [OnePair, ThreeOfAKind, FourOfAKind]
			= localHighestCard xs > localHighestCard ys
	| a1 == FullHouse
			= compareFullHouse xs ys
	| otherwise  = getHighFromTwoPairs xs > getHighFromTwoPairs ys

sortByLength = sortBy (comparing length)

sortGroups = sortByLength . group

localHighestCard =  head . last . sortGroups

getHighFromTwoPairs = maximum . drop 1 . concat . sortGroups

compareFullHouse xs ys = 
	case compare (localHighestCard xs) (localHighestCard ys) of
		GT	-> True
		LT	-> False
		_	-> getHighInPair xs > getHighInPair ys
	where
		getHighInPair = head . head . sortGroups

compareHand :: (Hand, Hand) -> Bool
compareHand (h1@(a1, xs), h2@(a2, ys))
	| a1 < a2 						= False
	| a1 > a2 						= True
	| a1 == RoyalFlush				= False
	| otherwise 					= 
		compareEqualHandType (h1, h2)

--Определение победителя
decideTheWinner :: [([Card], [Card])] -> [Bool]
decideTheWinner = map (compareHand . makeHand)
