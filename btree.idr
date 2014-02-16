module btree

data BTree a = Leaf | Node a (BTree a) (BTree a)

insert : Ord a => a -> BTree a -> BTree a
insert x Leaf = Node x Leaf Leaf
insert x (Node v l r) = if x < v then Node x (insert v l) r else Node x l (insert v r)

toList' : BTree a -> List a
toList' Leaf = []
toList' (Node x l r) = toList' l ++ [x] ++ toList' r

fromList : Ord a => List a -> BTree a
fromList [] = Leaf
fromList (x :: xs) = insert x (fromList xs)

