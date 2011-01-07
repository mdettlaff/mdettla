-- Maybe t = Nothing | Just t

add x Nothing = Nothing
add Nothing y = Nothing
add (Just x) (Just y) = Just (x + y)

pozycja :: Int -> [Int] -> Maybe Int
pozycja x [] = Nothing
pozycja x (y:ys) = if x == y then Just 0 else add (pozycja x ys) (Just 1)

my_drop :: Int -> [a] -> Maybe [a]
my_drop 0 [] = Just []
my_drop 0 (x:xs) = Just (x:xs)
my_drop n [] = Nothing
my_drop n (x:xs) = my_drop (n - 1) xs
