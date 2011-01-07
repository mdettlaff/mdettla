data BinTree a = Leaf a | Node (BinTree a) (BinTree a)

foldBinTree g f (Leaf x) = g x
foldBinTree g f (Node l r) = f (foldBinTree g f l) (foldBinTree g f r)

heightBinTree (Leaf a) = 1
heightBinTree (Node l r) = 1 + max (heightBinTree l) (heightBinTree r)

sumBinTree (Leaf a) = a
sumBinTree (Node l r) = (sumBinTree l) + (sumBinTree r)

tree1 = Leaf 1
tree2 = Node (Leaf 5) (Leaf 3)
tree3 = Node tree1 tree2
