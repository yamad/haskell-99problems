-- problem 19, rotate list N places left
import Criterion.Main

rotateLeft :: [a] -> Int -> [a]
rotateLeft xs n = take len $ drop (len + n) $ cycle xs
  where len = length xs

rotateLeftIterate :: [a] -> Int -> [a]
rotateLeftIterate [] _ = []
rotateLeftIterate xs 0 = xs
rotateLeftIterate xs n
  | n < 0     = rotateLeftIterate (last xs : (take (length xs - 1) xs)) (n+1)
  | otherwise = rotateLeftIterate (tail xs ++ [head xs]) (n-1)

main = do
  print $ rotateLeft ['a'..'h'] 3
  print $ rotateLeft ['a'..'h'] (-2)
  print $ rotateLeftIterate ['a'..'h'] 3
  print $ rotateLeftIterate ['a'..'h'] (-2)
  defaultMain [
    bgroup "rotate" [
        bench "rotateLeft" $ whnf (rotateLeft ['a'..'h']) 3
        , bench "rotateLeftIterate" $ whnf (rotateLeftIterate ['a'..'h']) 3
        , bench "rotateLeft" $ whnf (rotateLeft ['a'..'h']) (-2)
        , bench "rotateLeftIterate" $ whnf (rotateLeftIterate ['a'..'h']) (-2)
        ]
    ]
