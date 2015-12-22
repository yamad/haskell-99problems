-- problem 12, decode run-length encoding
import Criterion.Main
import Data.List

data RunEncodedElement = Single Char | Multiple Int Char deriving Show

decode :: [RunEncodedElement] -> String
decode []                = ""
decode (Single x:xs)     = [x] ++ decode xs
decode (Multiple l x:xs) = replicate l x ++ decode xs

decode' :: [RunEncodedElement] -> String
decode' = foldr f ""
  where f (Single x)     acc = x:acc
        f (Multiple l x) acc = (replicate l x) ++ acc

encode :: String -> [RunEncodedElement]
encode = map f . group
  where f [x]      = Single x
        f xs@(x:_) = Multiple (length xs) x

main = do
  let lst = "aaaabccaadeeee"
  print $ decode $ encode lst
  print $ decode' $ encode lst
  defaultMain [
    bgroup "decode" [ bench "d"  $ nf decode  $ encode lst
                    , bench "d'" $ nf decode' $ encode lst
                    ]
    ]
