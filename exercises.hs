myLast :: [a] -> a
myLast [] = error "Empty List"
myLast [x] = x
myLast (_:xs) = myLast xs

myButLast :: [a] -> a
myButLast [] = error "Empty List"
myButLast [x] = error "Insufficient"
myButLast xs = head (tail (reverse xs))

elementAt :: Int -> [a] -> a
elementAt 1 (x:_) = x
elementAt n (_:xs) = elementAt (n-1) xs
elementAt _ _ = error "Out of bounds!"

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse (xs)) ++ [x]

isPalindrome :: (Eq a)=>[a] -> Bool
isPalindrome [] = True
isPalindrome xs = reverse(xs) == xs

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a->[a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

compress1 :: (Eq a)=>[a]->[a]
compress1 [] = []
compress1 [x] = [x]
compress1 (x:xs) = x : (compress1 $ dropWhile (== x) xs)

pack :: (Eq a)=>[a] -> [[a]]
pack = foldr func []
	where func x [] = [[x]]
	      func x (y:xs) = if x == (head y) then ((x:y):xs) else ([x]:y:xs)

dupl :: [a] -> [a]
dupl [] = []
dupl (x:xs) = replicate 2 x ++ dupl(xs)

repl :: (Eq a)=>Int->[a]-> [a]
repl _ [] = []
repl n xs = replicate n x ++ repl n rem
	where
	  x = head(xs)
	  rem = tail(xs)

split :: [a]->Int->[[a]]
split [] _ = []
split xs n = first ++ second
	where
	  first = (take n xs):[]
	  second = reverse (take ((length xs) - n ) $ reverse xs) : []




