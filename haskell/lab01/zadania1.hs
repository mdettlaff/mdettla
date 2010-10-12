square a = a * a

call_with_5 f = f 5

same_values f1 f2 x y = f1 x y == f2 x y

is_odd 0 = False
is_odd n = is_even (n - 1)

is_even 0 = True
is_even n = is_odd (n - 1)
