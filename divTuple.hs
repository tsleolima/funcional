divTuple :: (Float,Float) -> Float

divTuple (_,0) = undefined
divTuple (x,y) = x / y
