-- problem 7, flatten a nested list
import Data.List

-- | type declaration, because Haskell lists are homogenous
data NestedList a = Elem a  | List [NestedList a] deriving Show

flatten :: NestedList a -> [a]
flatten = flat' []
  where
    flat' acc (Elem x )  = acc ++ [x]
    flat' acc (List xs)  = foldl' (\ax x -> flat' ax x) acc xs

-- concatMap maps a function over a list and concatenates results
flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List x) = concatMap flatten' x

main = do
  print $ (flatten (List []) :: [Bool])
  print $ flatten $ Elem 5
  print $ flatten $ List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]
  print $ (flatten' (List []) :: [Bool])
  print $ flatten' $ Elem 5
  print $ flatten' $ List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]
