data BTree a = NIL | Node [a] (BTree a) (BTree a) (BTree a) deriving (Eq,Show)

search' elem [] = False 
search' elem (x:xs)
 | elem == x = True
 | otherwise = search' elem xs

search elem NIL = False
search elem (Node chaves left mid rigth)
 | search' elem chaves = True
 | elem > maximum(chaves) = search elem rigth
 | elem < maximum(chaves) && elem > minimum(chaves) = search elem mid
 | otherwise = search elem left