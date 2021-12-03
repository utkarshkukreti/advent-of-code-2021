main :: IO ()
main = do
  xs <- map read . lines <$> getContents
  print $ p1 xs
  print $ p2 xs

p1 :: [Int] -> Int
p1 xs = length . filter id $ zipWith (<) xs (tail xs)

p2 :: [Int] -> Int
p2 xs = p1 $ zipWith3 (\a b c -> a + b + c) xs (tail xs) (tail $ tail xs)
