meupow = \x y -> x^y

fatorial = \x -> if(x==0) then 1 else x*fatorial(x-1)

isPrime = \a -> if(length [ x | x <- [2..(a-1)], mod a x == 0] == 0) then True
				else False

fib = \x -> if(x == 0) then 0 else
	  if(x == 1) then 1 else
	  fib (x-1) + fib(x-2)


mdc = \x y -> if(y == 0) then x else
	  mdc y (mod x y) 
