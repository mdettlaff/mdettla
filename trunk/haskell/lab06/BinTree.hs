data BinTree a = Leaf a | Node (BinTree a) (BinTree a) deriving Show

foldBinTree g f (Leaf x) = g x
foldBinTree g f (Node l r) = f (foldBinTree g f l) (foldBinTree g f r)

--heightBinTree (Leaf a) = 1
--heightBinTree (Node l r) = 1 + max (heightBinTree l) (heightBinTree r)
heightBinTree = foldBinTree (\ x -> 1) (\ x y -> 1 + max x y)

--sumBinTree (Leaf a) = a
--sumBinTree (Node l r) = (sumBinTree l) + (sumBinTree r)
sumBinTree = foldBinTree (\ x -> x) (+)

sizeBinTree = foldBinTree (\ x -> 1) (+)

mapBinTree f (Leaf x) = Leaf (f x)
mapBinTree f (Node l r) = Node (mapBinTree f l) (mapBinTree f r)

--preBinTree (Leaf x) = [x]
--preBinTree (Node l r) = (preBinTree l) ++ (preBinTree r)
preBinTree = foldBinTree (\ x -> [x]) (++)

tree1 = Leaf 1
tree2 = Node (Leaf 5) (Leaf 3)
tree3 = Node tree1 tree2
tree4 = Node tree1 (Leaf 4)
tree5 = Node (Node (Leaf 6) (Leaf 4)) tree3

--     tree3
--     /   \
--    1   tree2
--         /  \
--        5    3
