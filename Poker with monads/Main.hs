import Poker

main = do
	cards <- fmap words $ readFile "poker.txt"
	putStrLn $ "First player has won " ++ show (
				length $ filter (== P1Won) $ map decideTheWinner
						$ map (>>= makeHand)
						$ putToMonad
						$ readCardList cards) ++ " times"
