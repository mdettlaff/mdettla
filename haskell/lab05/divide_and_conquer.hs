-- divide and conquer
dc is_trivial end divide combine problem =
  if is_trivial problem
    then end problem
    else combine (map (dc is_trivial end divide combine)
                      (divide problem))


-- quicksort

split [] = []
split (a:as) =
  [[b | b <- as, b < a],
   [b | b <- as, b > a] ++ [a]]

flatten = foldl (++) []

quicksort :: Ord a => [a] -> [a]
quicksort = dc (\ xs -> length xs <= 1) (\ x -> x) split flatten


-- mergesort

ident x = x
halve xs = (\ pair -> [fst pair, snd pair]) (splitAt (div (length xs) 2) xs)

merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) =
  if x < y then x:(merge xs (y:ys)) else y:(merge (x:xs) ys)

mergesort :: Ord a => [a] -> [a]
mergesort = dc (\ xs -> length xs <= 1) ident halve (\ [xs, ys] -> merge xs ys)


-- Karatsuba algorithm

digits_count n = if -10 < n && n < 10 then 1 else 1 + digits_count (div n 10)

karatsuba_divide (a, b) =
  [(x1, y1), (x1, y0), (x0, y1), (x0, y0), (m, 1)]
  where m = div (digits_count a) 2; p = 10^m;
        x0 = mod a p; x1 = div a p; y0 = mod b p; y1 = div b p

karatsuba_combine ps =
  (ps !! 0) * 10^(2 * m) + ((ps !! 1) + (ps !! 2)) * 10^m + (ps !! 3)
  where m = ps !! 4

karatsuba a b = dc (\ (a, b) -> a < 10 || b < 10)
                   (\ (a, b) -> a * b)
                   karatsuba_divide
                   karatsuba_combine
                   (a, b)

