-- problem 13, run-length encoding without intermediate grouping into lists
import Data.List

data RunEncodedElement = Single Char | Multiple Int Char deriving Show

encodeDirect :: String -> [RunEncodedElement]
encodeDirect [] = []
encodeDirect (x:xs) = (makeRunElement (len+1) x) : encodeDirect rest
  where (len, rest) = spanCount (==x) xs

makeRunElement :: Int -> Char -> RunEncodedElement
makeRunElement 1 c = Single c
makeRunElement l c = Multiple l c

-- | returns the number of elements in prefix of [a] that match the
-- predicate.
spanCount :: (a -> Bool) -> [a] -> (Int, [a])
spanCount _    [] = (0, [])
spanCount pred xs@(x:xs')
  | pred x    = let (n, rest) = spanCount pred xs' in (n + 1, rest)
  | otherwise = (0, xs)

main = do
  let lst = "aaaabccaadeeee"
  let expected = [Multiple 4 'a', Single 'b', Multiple 2 'c',
                  Multiple 2 'a', Single 'd', Multiple 4 'e']
  print $ encodeDirect lst
  print expected
