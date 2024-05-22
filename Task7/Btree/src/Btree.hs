module Btree where 

data Tree a = Leaf | Node (Tree a) a (Tree a)


instance Foldable Tree where
    foldr f acc Leaf = acc
    foldr f acc (Node l v r) = foldr f (f v (foldr f acc r)) l


in_order :: Tree a -> [a]
in_order Leaf = []
in_order (Node l v r) = (in_order l) ++ (v : (in_order r))

pre_order :: Tree a -> [a]
pre_order Leaf = []
pre_order (Node l v r) =  v : (pre_order l ++ pre_order r)

post_order :: Tree a -> [a]
post_order Leaf = []
post_order (Node l v r) = (post_order l) ++ (post_order r) ++ [v]