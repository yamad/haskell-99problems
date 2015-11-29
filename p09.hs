-- problem 9, pack consecutive duplicates into sublists
import Data.List

-- | list version
pack :: (Eq a) => [a] -> [[a]]
pack []     = []
pack (x:xs) = (x : first) : pack rest
  where (first, rest) = span' (==x) xs

-- | accumulator version
pack' :: (Eq a) => [a] -> [[a]]
pack' = reverse . packAcc [] []
  where
    packAcc []  acc []     = []
    packAcc grp acc []     = (grp : acc)
    packAcc []  acc (x:xs) = packAcc [x] acc xs
    packAcc grp acc (x:xs)
      | x == head grp  = packAcc (x : grp) acc xs
      | otherwise      = packAcc [x] (grp : acc) xs

-- | return tuple (first, rest) with all elements at beginning of list
-- that satisfy predicate 'pred'. reimplementation of built-in 'span'
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' _  [] = ([], [])
span' pred xs@(x:xs')
  | pred x    = let (f, r) = span' pred xs' in (x:f, r)
  | otherwise = ([], xs)

-- | built-in 'group' works exactly like pack
packBuiltin = group

main = do
  let lst = "aaaabccaadeeee"
  print $ "should be "
  print $ ["aaaa", "b", "cc", "aa", "d", "eeee"]
  print $ pack lst
  print $ pack' lst
  print $ packBuiltin lst
  print $ (pack [] :: [[Bool]])
  print $ (pack' [] :: [[Bool]])
  print $ (packBuiltin [] :: [[Char]])
