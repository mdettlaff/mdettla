data Tree a = Node a [Tree a] deriving Show

tree3 = Node 3 [(Node 5 []), (Node 6 [])]
tree4 = Node 4 [Node 7 []]
tree1 = Node 1 [Node 2 [], tree3, tree4]

--     1
--   / | \
--  2  3  4
--    / \  \
--    5 6   7

mapTree f (Node n xs) = Node (f n) (map (mapTree f) xs)

foldTree g f (Node n xs) = foldl f (g n) (map (foldTree g f) xs)

--sizeTree (Node n xs) = 1 + sum (map sizeTree xs)
sizeTree = foldTree (\ x -> 1) (+)

sumTree = foldTree (\ x -> x) (+)

preTree = foldTree (\ x -> [x]) (++)
