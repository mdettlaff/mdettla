data Set a = EmptySet | Cons a (Set a) deriving Show

set1 = Cons 1 (Cons 5 (Cons 4 EmptySet))
set2 = Cons 2 (Cons 8 (Cons 7 EmptySet))
set3 = Cons 1 (Cons 4 (Cons 2 EmptySet))
set4 = Cons 1 (Cons 4 EmptySet)


member x EmptySet = False
member x (Cons head tail) = if head == x then True else member x tail

subset EmptySet s2 = True
subset (Cons head tail) s2 = (member head s2) && (subset tail s2)

union s EmptySet = s
union EmptySet s = s
union (Cons head tail) s2 =
    if member head s2
    then union tail s2
    else union tail (Cons head s2)

intersection s EmptySet = EmptySet
intersection EmptySet s = EmptySet
intersection (Cons head tail) s2 =
    if member head s2
    then (Cons head (intersection tail s2))
    else intersection tail s2

delete x EmptySet = EmptySet
delete x (Cons head tail) =
    if x == head
    then tail
    else Cons head (delete x tail)

