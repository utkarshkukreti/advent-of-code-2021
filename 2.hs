main :: IO ()
main = do
  xs <- map ((\[a, b] -> (a, read b)) . words) . lines <$> getContents
  print $ p1 xs
  print $ p2 xs

p1 :: [(String, Int)] -> Int
p1 xs = do
  uncurry (*) $ foldl step (0, 0) xs
  where
    step (x, y) ("forward", n) = (x + n, y)
    step (x, y) ("down", n) = (x, y + n)
    step (x, y) ("up", n) = (x, y - n)
    step _ _ = undefined

p2 :: [(String, Int)] -> Int
p2 xs = do
  let (x, y, aim) = foldl step (0, 0, 0) xs
  x * y
  where
    step (x, y, aim) ("forward", n) = (x + n, y + aim * n, aim)
    step (x, y, aim) ("down", n) = (x, y, aim + n)
    step (x, y, aim) ("up", n) = (x, y, aim - n)
    step _ _ = undefined
