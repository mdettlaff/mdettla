-- divide and conquer
dc test end divide comb p =
  if test p
    then end p
    else comb (map (dc test end divide comb)
                   (divide p))


split [] = []
split (a:as) =
  [[b | b <- as, b < a],
   [b | b <- as, b > a] ++ [a]]

flatten = foldl (++) []

quicksort :: Ord a => [a] -> [a]
quicksort = dc (\ xs -> length xs <= 1) (\ x -> x) split flatten


ident x = x
halve xs = (\ pair -> [fst pair, snd pair]) (splitAt (div (length xs) 2) xs)

merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) =
  if x < y then x:(merge xs (y:ys)) else y:(merge (x:xs) ys)

mergesort :: Ord a => [a] -> [a]
mergesort = dc (\ xs -> length xs <= 1) ident halve (\ [xs, ys] -> merge xs ys)
