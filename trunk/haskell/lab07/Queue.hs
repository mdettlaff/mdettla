data ConsQueue a = EmptyQueue | Cons a (ConsQueue a) deriving Show
data ListQueue a = LQ [a] deriving Show

class Queue q where
    isEmptyQueue :: q a -> Bool
    enQueue :: a -> q a -> q a
    deQueue :: q a -> q a
    getFrontQueue :: q a -> a
    showQueue :: (Show a) => q a -> String
    showQueue xs =
        if isEmptyQueue xs then ""
        else " -> " ++ (show (getFrontQueue xs)) ++ (showQueue (deQueue xs))

instance Queue ConsQueue where
    isEmptyQueue EmptyQueue = True
    isEmptyQueue _ = False
    enQueue x EmptyQueue = Cons x EmptyQueue
    enQueue x (Cons y ys) = Cons y (enQueue x ys)
    deQueue (Cons x xs) = xs
    getFrontQueue (Cons x xs) = x

instance Queue ListQueue where
    isEmptyQueue (LQ xs) = length xs == 0
    enQueue x (LQ xs) = LQ (xs ++ [x])
    deQueue (LQ (x:xs)) = LQ (xs)
    getFrontQueue (LQ xs) = head xs

q1 = EmptyQueue
q2 = Cons 3 (Cons 1 (Cons 2 EmptyQueue))
lq1 = LQ []
lq2 = LQ [3, 1, 2]
