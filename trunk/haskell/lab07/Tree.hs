data Tree a = Node a [Tree a] deriving Show

tree3 = Node 3 [(Node 5 []), (Node 6 [])]
tree4 = Node 4 [Node 7 []]
tree1 = Node 1 [Node 2 [], tree3, tree4]

--     1
--   / | \
--  2  3  4
--    / \  \
--    5 6   7

sizeTree (Node n []) = 1
sizeTree (Node n (x:xs)) = 1 + (sizeTree x) + sum (map sizeTree xs)
