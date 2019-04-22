module RevisaoProva (
		List(..),
		BinaryTree(NIL, Node),
		xor,
		impl,
		square,
		sizeBST,
		pow,
		listLength,
		printList,
		listHead,
		listTail,
		listLast,
		listFoldr,
		listFoldl
	)

where

-- Exercicios Introdutorios  --

xor :: Bool -> Bool -> Bool
xor a b = ((not a) && b) || (a && (not b))

impl :: Bool -> Bool -> Bool
impl a b = ((not a) || b)

equiv :: Bool -> Bool -> Bool
equiv a b = (impl a b) && (impl b a)

square :: Int -> Int
square x = x*x

sumSquares x y = square x + square y

pow :: Float -> Float -> Float
pow x 0 = 1
pow x y | y > 0 = (x * pow x (y-1))
		| otherwise = 1 / pow x (-y)

fatorial :: Int -> Int
fatorial 0 = 1
fatorial x = x * fatorial (x-1)

isPrime :: Int -> Bool
isPrime y | length [x | x <- [2..(y-1)], mod y x == 0] > 0 = False
		  | otherwise = True


fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib(x-2)

mdc :: Int -> Int -> Int
mdc 0 y = y
mdc x 0 = x
mdc x y = mdc y r
	where r = mod x y

coprimo :: Int -> Int -> Bool
coprimo x y | mdc x y == 1 = True
			| otherwise = False

divTuple :: (Float, Float) -> Float 
divTuple (x, 0) = undefined
divTuple (x, y) = x/y

somatorio a b | a > b = undefined
			  | otherwise = sum [a..b]

somatorioRec a b | a > b = undefined
			     | otherwise = if(a == b) then a else a + somatorio (a+1) b 

higherOrderSum f a b = sum (map f [a..b])

hoSumSquares = higherOrderSum square

mapFilter f p xs = filter p (map f xs)

-- Exercicios sobre Listas --

meuLast [] = error "Lista Vazia!"
meuLast [x] = x
meuLast (x:xs) = meuLast xs

penultimo [] = error "Lista sem Penultimo!"
penultimo [x] = error "Lista sem Penultimo!"
penultimo xs = last (init xs)   

elementAt 1 xs = head xs
elementAt i xs = elementAt (i-1) (tail xs) 

meuLength [] = 0
meuLength (x:xs) = 1 + meuLength xs

meuReverso [] = []
meuReverso (x:xs) = meuReverso xs ++ [x]

isPalindrome xs | meuReverso xs == xs = True
				| otherwise = False 

isPalindrome2 [] = True
isPalindrome2 [x] = True
isPalindrome2 xs | (head xs == last xs) = isPalindrome2 (init(tail xs))
	 			 | otherwise = False


buildPalindrome [] = []
buildPalindrome (x:xs) = [x] ++ buildPalindrome xs ++ [x]

compress [] = []
compress xs = compress (init xs) ++ [y | y <-[last xs], not (elem y (init xs)) ]

insertAt elem pos xs = (take (pos-1) xs) ++ [elem] ++ (drop (pos-1) xs)

minList [x] = x
minList (x:xs) | x < (minList xs) = x
			   | otherwise = minList xs

-- Exercicios sobre Calculos Lambda --

factLambda = \x -> if(x == 0) then 1 else x * factLambda (x-1) 

powLambda = \x y -> if(y == 0) then 1 else if (y > 0) then x * powLambda x (y-1) else 1/powLambda x (-y)

squareLambda = \x -> x * x

isPrimeLambda = \x -> if(length [y | y <- [2..(x-1)], mod x y == 0] > 0) then False else True

fibLambda = \x -> if(x == 0) then 1 else if (x == 1) then 1 else fibLambda (x-1) + fibLambda (x-2)

mdcLambda = \x y -> if(x == 0) then y else if (y == 0) then x else mdc y (mod x y)

coprimoLambda = \x y -> if(mdc x y == 1) then True else False

-- Exercicios sobre Tipos de dados --

data Triple a b c = Triple a b c deriving  (Eq,Show)

tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c

data Quadruple a b = Quadruple a a b b deriving (Eq,Show)

firstTwo (Quadruple a b c d) = (a,b)
secondTwo (Quadruple a b c d) = (c,d)

data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d deriving (Eq,Show)

tuple1 (Tuple1 a) = Just a
tuple1 (Tuple2 a b) = Just a
tuple1 (Tuple3 a b c) = Just a
tuple1 (Tuple4 a b c d) = Just a

tuple2 (Tuple2 a b) = Just b
tuple2 (Tuple3 a b c) = Just b
tuple2 (Tuple4 a b c d) = Just b
tuple2 _ = Nothing

tuple3 (Tuple3 a b c) = Just c
tuple3 (Tuple4 a b c d) = Just c
tuple3 _ = Nothing

tuple4 (Tuple4 a b c d) = Just d
tuple4 _ = Nothing

-- Tipos de dados Recusivos --

data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

printList Nil = []
printList (Cons x xs) = x:(printList xs)

listHead Nil = error "Lista Vazia!"
listHead (Cons x xs) = x

listTail Nil = error "Lista Vazia!"
listTail (Cons x xs) = xs

listLast Nil = error "Lista Vazia!"
listLast (Cons x Nil) = x
listLast (Cons x xs) = listLast xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)

listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs

data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a) deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right