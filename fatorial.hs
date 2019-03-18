fatorial :: Int -> Int
fatorial 0 = 1
fatorial x = x * fatorial (x-1)