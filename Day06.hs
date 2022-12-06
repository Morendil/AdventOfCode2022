import Data.List (tails, nub, transpose)

windows :: Int -> [a] -> [[a]]
windows n = transpose . take n . tails

part1 :: String -> Int
part1 = (+4) . length . takeWhile (\s -> (length $ nub s) /= 4) . windows 4

main = do
    signal <- lines <$> readFile "day06.txt"
    mapM_ (print . part1) signal