--my_filter f [] = []
--my_filter f (x:xs) = (if f x then [x] else []) ++ my_filter f xs
my_filter f xs = [x | x <- xs, f x]

my_foldl f z [] = z
my_foldl f z (x:xs) = my_foldl f (f z x) xs

my_foldr f z [] = z
my_foldr f z (x:xs) = f x (my_foldr f z xs)

my_length = my_foldl (\ x y -> x + 1) 0
my_and = my_foldl (&&) True
my_sum = my_foldl (+) 0
my_prod = my_foldl (*) 1
my_map f = my_foldr ((:) . f) []
my_reverse = my_foldr (\ x y -> y ++ [x]) []

my_zip_with f [] _ = []
my_zip_with f (x:xs) (y:ys) = (f x y):(my_zip_with f xs ys)

prod v1 v2 = my_foldl (+) 0 (my_zip_with (*) v1 v2)

--flatten [] = []
--flatten (x:xs) = x ++ (flatten xs)
flatten = my_foldl (++) []

insertion_sort [] = []
insertion_sort (x:xs) = insert x (insertion_sort xs)
    where insert x xs = [y | y <- xs, y < x] ++ [x] ++ [y | y <- xs, x <= y]

