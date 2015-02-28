import Poker

main = do
	cards <- fmap words $ readFile "poker.txt"
	putStrLn $ "First player has won " ++ show (
				length $ filter (== True) $ decideTheWinner 
						$ readCardList cards) ++ " times"
