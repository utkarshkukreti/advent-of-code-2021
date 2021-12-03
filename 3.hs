import Data.List (transpose)

main :: IO ()
main = do
  xs <- map (map (== '1')) . lines <$> getContents
  print $ p1 xs
  print $ p2 xs

p1 :: [[Bool]] -> Int
p1 xs = go mostFrequent * go leastFrequent
  where
    go f = toInt . map f $ transpose xs

p2 :: [[Bool]] -> Int
p2 xs = go mostFrequent * go leastFrequent
  where
    go f = toInt . head $ foldl go2 xs [0 .. length (head xs) - 1]
      where
        go2 [x] _ = [x]
        go2 xs i = do
          let x = f $ map (!! i) xs
          filter ((== x) . (!! i)) xs

mostFrequent :: [Bool] -> Bool
mostFrequent xs = length (filter id xs) >= length (filter not xs)

leastFrequent :: [Bool] -> Bool
leastFrequent = not . mostFrequent

toInt :: [Bool] -> Int
toInt = foldl (\acc x -> acc * 2 + if x then 1 else 0) 0
