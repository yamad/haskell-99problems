-- problem 10, return run-length encoding
import Data.List

-- | map-based point-free style
encode :: Eq a => [a] -> [(Int, a)]
encode = map f . group
  where f = (\x -> (length x, head x))

-- | list comprehension style
encodeListComp :: Eq a => [a] -> [(Int, a)]
encodeListComp xs = [(length x, head x) | x <- group xs]

main = do
  let lst = "aaaabccaadeeee"
  print $ encode lst
  print $ encodeListComp lst
