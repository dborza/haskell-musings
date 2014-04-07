module RandomTest
(	quicksort
,	collatz
,	collatz_chain
,	collatz_chain_sum
,	min'
,	min_list
,	elem'
,	map'
)	where

-- ye olde quicksorte
quicksort :: (Integral a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) = quicksort (filter (<=x) xs) ++ [x] ++ quicksort (filter (>x) xs)

-- Some funky function, also known at 'Collatz' function
collatz :: (Integral a) => a -> a
collatz x = if (even x) then (x `div` 2) 
			else (3 * x + 1)

--	Defines a collatz chain, given an integer. Supposedly all collatz chains end in '1'.	
collatz_chain :: (Integral a) => a -> [a]
collatz_chain 1 = [1]
collatz_chain x = x : (collatz_chain (collatz x))

-- How many collatz_chains starting from 1 .. n have a length greater than 15?
collatz_chain_sum :: (Integral a) => a -> Int
collatz_chain_sum x = length (filter (>15) (map length (map collatz_chain [1..x])))
 
min' :: (Integral a) => a -> a -> a
min' x y = if (x <= y) then x 
			else y

-- determine the minimum from a list
min_list :: (Integral a) => [a] -> a
min_list [x] = x
min_list (x:hx) = min' x (min_list hx)
	
-- another definition of the elem function. Checks if a given element is part of a list
elem' e list = foldl (\acc x -> acc || x == e) False list

-- implement the map function using a fold	
map' f list = foldl (\acc x -> acc ++ [f(x)]) [] list