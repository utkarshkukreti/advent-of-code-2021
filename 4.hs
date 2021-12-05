import Data.List (groupBy, inits, partition, transpose, (\\))

type Board = [[Int]]

type Draw = [Int]

main :: IO ()
main = do
  ([a] : b) <- map (filter (not . null)) . groupBy (\a b -> null a && not (null b)) . lines <$> getContents
  let draw = map read . words . map (\c -> if c == ',' then ' ' else c) $ a
  let boards = map (map (map read . words)) b
  let winners = run draw boards
  print $ snd . head $ winners
  print $ snd . last $ winners

run :: Draw -> [Board] -> [(Board, Int)]
run draw boards = fst $ foldl go ([], boards) (inits draw)
  where
    go :: ([(Board, Int)], [Board]) -> Draw -> ([(Board, Int)], [Board])
    go (won, left) drawn = do
      let (won', left') = partition (isWon drawn) left
      let won'' = map (\board -> (board, score drawn board)) won'
      (won ++ won'', left')

score :: Draw -> Board -> Int
score drawn board = (sum . (\\ drawn) . concat $ board) * last drawn

isWon :: Draw -> Board -> Bool
isWon drawn board = go board || go (transpose board)
  where
    go = any (all (`elem` drawn))
