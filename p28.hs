-- problem 28, sort list of lists by length
import Data.List
import Data.Ord (comparing)
import Data.Function (on)

-- | sort list of lists by ascending length
lsort :: [[a]] -> [[a]]
lsort     [] = []
lsort (x:xs) = insert' (compareLength) x $ lsort xs
  where compareLength = (\a b -> length a <= length b)

-- | insertion sort. takes a comparison function
insert' :: (a -> a -> Bool) -> a -> [a] -> [a]
insert' _   x []         = [x]
insert' cmp x ys@(y:ys')
  | cmp x y   = x : ys
  | otherwise = y : insert' cmp x ys'

-- | sort list of lists by length frequency
lfsort :: [[a]] -> [[a]]
lfsort xs = sortBy (comparing (flip elemIndex order . length)) xs
  where order  = map snd $ sort counts
        counts = [ (length g, head g) | g <- group $ sort $ map length xs]

-- | sort list of lists by length frequency. nice solution from wiki,
-- making use of @lsort@ for sorting elements by length.
--
-- the first @lsort@ collects equal length elements next to each
-- other. then @groupBy@ partitions the list by grouping elements with
-- the same length (now adjacent to each other). the second @lsort@ on
-- the list of lists is the really cool move, because it orders the
-- groups by number of members. the same function used twice in
-- different contexts on the same data.
lfsort' :: [[a]] -> [[a]]
lfsort' = concat . lsort . groupBy ((==) `on` length) . lsort

-- | TODO
main = do
  let list = ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
  print $ lsort list
  print $ (lsort [] :: [String])
  print $ lfsort list
