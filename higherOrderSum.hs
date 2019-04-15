higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum f a b = sum (map f [a,b])
