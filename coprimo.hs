mdc :: Int -> Int -> Int 
mdc a b | b == 0 = a
		| otherwise = mdc b (mod a b)

coprimo :: Int -> Int -> Bool
coprimo x y | mdc x y == 1 = True
			| otherwise = False