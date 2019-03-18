isPrime :: Int -> Bool
isPrime b | (length (countPrime b) > 0) = False
          | otherwise = True

countPrime :: Int -> [Int]
countPrime a = [ x | x <- [2..(a-1)], mod a x == 0]
			