isPrime :: Int -> Bool
isPrime b | (length (countPrime b) > 0) = False
          | otherwise = True

countPrime :: Int -> [Int]
countPrime a = [ x | x <- [2..(a-1)], mod a x == 0]
			
goldbach :: Int -> [(Int,Int)]
goldbach x = [(y,z) | y <- filter isPrime [1..(x-1)], z <- filter isPrime [1..(x-1)], y + z == x]