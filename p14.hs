-- problem 14, duplicate the elements of a list
import Data.List

-- | direct version
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x:x:duplicate xs

-- | fold-based version
duplicateFold :: [a] -> [a]
duplicateFold = foldr (\x acc -> x:x:acc) []

-- | monad-based version
duplicateMonad :: [a] -> [a]
duplicateMonad = (=<<) (\x -> [x,x])

duplicateList :: [a] -> [a]
duplicateList = concat . map (\x -> [x,x])

main = do
  print $ take 20 $ duplicate [1..]
  print $ take 20 $ duplicateFold [1..]
  print $ take 20 $ duplicateMonad [1..]
  print $ take 20 $ duplicateList [1..]
