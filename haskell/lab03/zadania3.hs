append [] xs = xs
append (x:xs) ys = x:(append xs ys)

member x [] = False
member x (y:ys) = if x == y then True else member x ys

my_reverse [] = []
my_reverse (x:xs) = append (my_reverse xs) [x]

my_last [x] = x
my_last (x:xs) = my_last xs

--delete x [] = []
--delete x (y:ys) = if x == y then delete x ys else y:(delete x (ys))
delete x ys = [y | y <- ys, not (y == x)]

my_map f [] = []
my_map f (x:xs) = (f x):(my_map f xs)
--my_map f = foldr ((:) . f) [] -- head == explode!

square x = x * x
-- przykład zastosowania currying
--mapper f = \ xs -> my_map f xs
mapper f = \ xs -> if xs == [] then [] else (f (head xs)):((mapper f) (tail xs))
-- funkcja podnosząca do kwadratu każdy element podanej listy
--squarer = mapper square
squarer = my_map square

permutations [] = [[]]
permutations xs = [x:ys | x <- xs, ys <- (permutations (delete x xs))]

pythagorean_triple n =
    [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

iter 0 f = (\ x -> x)
iter n f = f.(iter (n - 1) f)
