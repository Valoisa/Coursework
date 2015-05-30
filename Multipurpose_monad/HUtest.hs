import Test.HUnit
import Card
import GameClass
import GameMonad
import Poker
import Quidditch
import Hearts
import Control.Applicative

extractPokerWinner = length . (filter (== 1)) . map decideWinner
						. (map (>>= makeHand))
						. putToMonad
						. readCardList

extractQudditchWinner = length . (filter (== 1)) 
						 . map decideWinner . toMonad
						 . stringToListOfGames
extractHeartsWinner :: [String] -> Int
extractHeartsWinner = length . (filter (== 1)) . map decideWinner
						. placeToMonad . stringToListOfBribes

msgPoker = "Should be 376 for the first player"
pokerTest = TestCase $ 
				(words <$> readFile "poker.txt") >>= 
					assertEqual msgPoker 376 . extractPokerWinner

msgHearts = "Should be 1 for the first player"
heartsTest = TestCase $
				(lines <$> readFile "hearts.txt") >>=
					assertEqual msgHearts 1 . extractHeartsWinner

msgQuidditch = "Should be 4 for the first player"
quidditchTest = TestCase $
				(lines <$> readFile "quidditch.txt") >>=
					assertEqual msgQuidditch 6 . extractQudditchWinner

testAll = TestList [TestLabel "PokerTest" pokerTest
					, TestLabel "QuidditchTest" quidditchTest
					, TestLabel "HeartsTest" heartsTest]

main = runTestTT testAll
