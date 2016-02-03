-- problem 27, group the elements of a set into disjoint subsets
--
-- really struggled with this one and needed to look at solutions a
-- lot. the code here ended up being a copy of the solution code on
-- the wiki. the elegant use of list comprehensions here is worth
-- studying.

-- | return disjoint subsets of a list, where the second argument
-- gives the lengths of the desired subsets
group' :: [a] -> [Int] -> [[[a]]]
group' _ []  = [[]]
group' xs (n:ns) = [ g:gs | (g, ys) <- combos n  xs,
                                gs  <- group' ys ns ]

combos :: Int -> [a] -> [([a], [a])]
combos 0 xs     = [([], xs)]
combos _ []     = []
combos n (x:xs) = ts ++ ds
  where ts = [ (x:ys,   zs) | (ys, zs) <- combos (n-1) xs ]
        ds = [ (  ys, x:zs) | (ys, zs) <- combos  n    xs ]

main = do
  let names = ["aldo", "beat", "carla", "david", "evi", "flip", "gary", "hugo", "ida"]
  print $ length $ group' names [2,3,4] -- 1260
  print $ length $ group' names [2,2,5] -- 756
