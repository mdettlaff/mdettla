data ConsQueue a = EmptyQueue | Cons a (ConsQueue a) deriving Show

class Queue q where
    type Elem q :: *
    isEmptyQueue :: q -> Bool
    enQueue :: Elem q -> q -> q
    deQueue :: q -> q
    getFrontQueue :: q -> Elem q

instance Queue (ConsQueue a) where
    type Elem (ConsQueue a) = a
    isEmptyQueue EmptyQueue = True
    isEmptyQueue _ = False
    enQueue x xs = Cons x xs
    deQueue (Cons x xs) = xs
    getFrontQueue (Cons x xs) = x


q1 = EmptyQueue
q2 = Cons 3 (Cons 1 (Cons 2 EmptyQueue))
