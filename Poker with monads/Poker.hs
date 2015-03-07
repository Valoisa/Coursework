module Poker where
import Data.List
import Data.Ord (comparing)
import Control.Monad

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

data Game a = BeforeGame a | P1Won | P2Won 
				deriving (Show, Eq, Ord, Read)

instance Functor Game where
	fmap f (BeforeGame a) 	= BeforeGame (f a)
	fmap f P1Won			= P1Won
	fmap f P2Won			= P2Won
	
instance Monad Game where
	return x 			= BeforeGame x
	P1Won >>= f 		= P1Won
	P2Won >>= f 		= P2Won
	BeforeGame x >>= f 	= f x

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

putToMonad :: [([Card], [Card])] -> [Game ([Card], [Card])]
putToMonad = map return

--Составление пары ставок
makeHand :: ([Card], [Card]) -> Game (Hand, Hand)
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
								P1Won else P2Won

secondDecideTheWinner xs ys = 
						if localHighestCard xs > localHighestCard ys
						then P1Won else P2Won

firstCheck :: (Hand, Hand) -> Game (Hand, Hand)
firstCheck hand@((a1, xs), (a2, ys)) 
	| a1 `elem` [HighCard, Flush, Straight, StraightFlush]
				= firstDecideTheWinner xs ys
	| otherwise = return hand

secondCheck :: (Hand, Hand) -> Game (Hand, Hand)
secondCheck hand@((a1, xs), (a2, ys))
	| a1 `elem` [OnePair, ThreeOfAKind, FourOfAKind]
				= secondDecideTheWinner xs ys
	| otherwise = return hand

thirdCheck :: (Hand, Hand) -> Game (Hand, Hand)
thirdCheck hand@((a1, xs), (a2, ys))
	| a1 == FullHouse 	= compareFullHouse xs ys
	| otherwise 		= return hand

compareFullHouse xs ys = 
	case compare (localHighestCard xs) (localHighestCard ys) of
		GT	-> P1Won
		LT	-> P2Won
		_	-> if getHighInPair xs > getHighInPair ys then P1Won 
				else P2Won
	where
		getHighInPair = head . head . sortGroups

compareHand :: (Hand, Hand) -> Game (Hand, Hand)
compareHand (h1@(a1, xs), h2@(a2, ys))
	| a1 < a2 			= P2Won
	| a1 > a2 			= P1Won
	| a1 == RoyalFlush	= P2Won
	| otherwise 		= return (h1, h2)

decideTheWinner :: Game (Hand, Hand) -> Game (Hand, Hand)
decideTheWinner a = 
					a >>= compareHand 
					>>= firstCheck
					>>= secondCheck
					>>= thirdCheck
