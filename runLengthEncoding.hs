import Data.List

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x->(length x, head x)) (group xs)

{-
Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists
-}

data ListItem a = Single a | Multiple Int a deriving(Show)
modifiedEncode :: (Eq a) => [a] -> [ListItem a]
modifiedEncode = map modifiedEncode . encode 
	where
	  modifiedEncode (1,x) = Single x
	  modifiedEncode (n,x) = Multiple n x

modifiedDecode :: [ListItem a] -> [a]
modifiedDecode = concatMap dHelper
	where
	 dHelper (Single x) = [x]
	 dHelper (Multiple n x) = replicate n x
