dc test end divide comb p =
  if (test p)
    then (end p)
    else comb ((map (dc test end divide comb)
                    (divide p)))

split [] = []
split (a:as) =
  [[b | b <- as, b < a],
   [b | b <- as, b > a] ++ [a]]

flatten = foldl (++) []

quicksort :: Ord a => [a] -> [a]
quicksort = dc (\ l -> length l <= 1) (\ x -> x) (split) (flatten)
