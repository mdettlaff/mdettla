nwd a 0 = a
nwd a b = nwd b (mod a b)

nww a b = div (a * b) (nwd a b)

delta a b c = b^2 - 4 * a * c
kwad a b c = ((-b - sqrt (delta a b c)) / (2 * a),
    (-b + sqrt (delta a b c) / (2 * a)))

fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

hanoi 0 _ _ _ = []
hanoi n from to using =
    hanoi (n - 1) from using to ++
    [(from, to)] ++
    hanoi (n - 1) to using from

fac 0 = 1
fac n = fac (n - 1) * n

binom _ 0 = 1
binom n k = if n == k then 1 else binom (n - 1) (k - 1) + binom (n - 1) k
binom_non_recursive n k = div (fac n) (fac k * fac (n - k))

nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])
pascal 0 = [1]
pascal n = nextRow (pascal (n - 1))
