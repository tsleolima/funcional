somatorioRec :: Int -> Int -> Int
somatorioRec x y | x > y = undefined
				 | otherwise = somatorioRec' [x..y]

somatorioRec' :: [Int] -> Int
somatorioRec' [] = 0
somatorioRec' (x:xs) = x + somatorioRec' xs