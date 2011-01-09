data ConsQueue a = EmptyQueue | Cons a (ConsQueue a) deriving Show
data ListQueue a = LQ [a] deriving Show

class Queue q where
    isEmptyQueue :: q a -> Bool
    enQueue :: a -> q a -> q a
    deQueue :: q a -> q a
    getFrontQueue :: q a -> a

instance Queue ConsQueue where
    isEmptyQueue EmptyQueue = True
    isEmptyQueue _ = False
    enQueue x xs = Cons x xs
    deQueue (Cons x EmptyQueue) = EmptyQueue
    deQueue (Cons x xs) = Cons x (deQueue xs)
    getFrontQueue (Cons x xs) = x

instance Queue ListQueue where
    isEmptyQueue (LQ xs) = length xs == 0
    enQueue x (LQ xs) = LQ (x:xs)
    deQueue (LQ xs) = LQ (remove_last xs)
    getFrontQueue (LQ xs) = head xs

remove_last [x] = []
remove_last (x:xs) = x:(remove_last xs)

q1 = EmptyQueue
q2 = Cons 3 (Cons 1 (Cons 2 EmptyQueue))
lq1 = LQ []
lq2 = LQ [3, 1, 2]
