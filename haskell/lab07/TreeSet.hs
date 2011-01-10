import Tree
import Set

data TreeSet a = Tree (Set a) [Tree (Set a)]

ts1 = Node (Cons 1 EmptySet) []
ts2 =
    Node (Cons 1 (Cons 2 EmptySet))
        [Node (Cons 2 (Cons 3 EmptySet))
            [Node (Cons 2 (Cons 1 (Cons 5 EmptySet))) []],
         Node (Cons 1 EmptySet) []]


deleteTree x = mapTree (\ s -> delete x s)

unionTree = foldTree (\ s -> (setToList s)) (++)
