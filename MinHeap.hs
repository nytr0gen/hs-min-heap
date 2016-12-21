module MinHeap(MinHeap, empty, null,
    head, length, add, tail,
    toList, fromList)
where

import Prelude hiding (null, head, length, tail)

data MinHeap a = Leaf
                | Node a (MinHeap a) (MinHeap a) Int

toList :: Ord a => MinHeap a -> [a]
toList Leaf = []
toList a = head a:toList (tail a)

fromList :: Ord a => [a] -> MinHeap a
fromList = foldr add empty

instance (Ord a, Show a) => Show (MinHeap a) where
    show = show . toList

-- example for testing
root = Node 1
    (Node 3 (Node 7 Leaf Leaf 1) (Node 5 Leaf Leaf 1) 3)
    (Node 2 (Node 6 Leaf Leaf 1) (Node 4 (Node 8 Leaf Leaf 1) Leaf 2) 4)
    8

empty :: MinHeap a
empty = Leaf

null :: MinHeap a -> Bool
null Leaf = True
null _ = False

head :: MinHeap a -> a
head (Node e _ _ _) = e

length :: MinHeap a -> Int
length Leaf = 0
length (Node _ _ _ l) = l

add :: Ord a => a -> MinHeap a -> MinHeap a
add v Leaf = Node v Leaf Leaf 1
add v (Node e l r len)
    | v < e               = add e (Node v l r len)
    | length l < length r = Node e (add v l) r (len+1)
    | otherwise           = Node e l (add v r) (len+1)

replace :: Ord a => a -> MinHeap a -> MinHeap a
replace v (Node e Leaf Leaf len) = Node v Leaf Leaf len
replace v (Node e l Leaf len)
    | v < el = Node v l Leaf len
    | otherwise = Node el (replace v l) Leaf len
    where el = head l
replace v (Node e Leaf r len)
    | v < er = Node v Leaf r len
    | otherwise = Node er Leaf (replace v r) len
    where er = head r
replace v (Node e l r len)
    | v < el && v < er = Node v l r len
    | el < er   = Node el (replace v l) r len
    | otherwise = Node er l (replace v r) len
    where el = head l
          er = head r

extractFinal :: Ord a => MinHeap a -> (a, MinHeap a)
extractFinal a = (get_f a, get_a' a)
    where
        get_f (Node e Leaf Leaf _) = e
        get_f (Node e l r _)
            | length l < length r = get_f r
            | otherwise           = get_f l
        get_a' (Node e Leaf Leaf len) = Leaf
        get_a' (Node e l r len)
            | length l < length r = Node e l (get_a' r) len
            | otherwise           = Node e (get_a' l) r len

tail :: Ord a => MinHeap a -> MinHeap a
tail Leaf = Leaf
tail a
    | null (snd fin) = Leaf
    | otherwise      = replace (fst fin) (snd fin)
    where fin = extractFinal a
