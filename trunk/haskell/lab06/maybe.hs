-- Maybe t = Nothing | Just t

f x Nothing = Nothing
f Nothing y = Nothing
f (Just x) (Just y) = Just (x + y)

pozycja :: Int -> [Int] -> Maybe Int
pozycja x [] = Nothing
pozycja x (y:ys) = if x == y then Just 0 else f (pozycja x ys) (Just 1)

