data Triple a b c = Triple a b c deriving (Eq, Show)

tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c

data Quadruple a b = Quadruple a a b b deriving (Eq, Show)

firstTwo (Quadruple a b c d) = (a,b)
secondTwo (Quadruple a b c d) = (c,d)

data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d deriving (Eq, Show)

-- Necessario tratar todos os construtores

tuple1 (Tuple1 a ) = Just a
tuple1 (Tuple2 a b) = Just a
tuple1 (Tuple3 a b c) = Just a
tuple1 (Tuple4 a b c d) = Just a

tuple2 (Tuple2 a b) = Just b
tuple2 (Tuple3 a b c) = Just b
tuple2 (Tuple4 a b c d) = Just b

tuple3 (Tuple3 a b c) = Just c
tuple3 (Tuple4 a b c d) = Just c

tuple4 (Tuple4 a b c d) = Just d

data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = error "Empty list"
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)


listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 


data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
 deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

pegaValor (Node a left right) = a

isBST NIL = True
isBST (Node valor NIL NIL) = True
isBST (Node valor _ NIL) = True
isBST (Node valor NIL _) = True
isBST (Node valor left right) = 
	if((valor < pegaValor left) || (valor > pegaValor right))
	then False
	else (isBST left) && (isBST right) 

insert novoValor NIL = (Node novoValor NIL NIL) 
insert novoValor (Node valor left right) = 
	if (novoValor < valor)
		then (Node valor (insert novoValor left) right)
		else (Node valor left (insert novoValor right))

search valorBuscado NIL = NIL
search valorBuscado (Node valor left right) =
	if (valorBuscado == valor)
		then (Node valor left right)
		else if (valorBuscado < valor)
			 then search valorBuscado left
			 else search valorBuscado right 

maximo (Node valor NIL NILL) = valor
maximo (Node valor left NIL) = valor
maximo (Node valor left right) = maximo right

minimo (Node valor NIL NILL) = valor
minimo (Node valor NIL right) = valor
minimo (Node valor left right) = minimo right

sucessor elem NIL = NIL
sucessor elem (Node valor left right) = minimo right  