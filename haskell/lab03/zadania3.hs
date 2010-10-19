append [] xs = xs
append (x:xs) ys = x:(append xs ys)

member x [] = False
member x (y:ys) = if x == y then True else member x ys

my_reverse [] = []
my_reverse (x:xs) = append (my_reverse xs) [x]

my_map f [] = []
my_map f (x:xs) = (f x):(my_map f xs)

square x = x * x
-- przykład zastosowania currying
--mapper f = \ xs -> my_map f xs
mapper f = \ xs -> if xs == [] then [] else (f (head xs)):((mapper f) (tail xs))
-- funkcja podnosząca do kwadratu każdy element podanej listy
squarer = mapper square
