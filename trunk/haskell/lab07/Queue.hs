data ConsQueue a = EmptyQueue | Cons a (ConsQueue a) deriving Show

class Queue a where
    isEmptyQueue :: a -> Bool

instance Queue (ConsQueue a) where
    isEmptyQueue EmptyQueue = True
    isEmptyQueue _ = False


q1 = EmptyQueue
q2 = Cons 3 (Cons 1 (Cons 2 EmptyQueue))
