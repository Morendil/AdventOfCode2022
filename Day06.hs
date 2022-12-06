import Data.List (tails, nub, transpose)

windows :: Int -> [a] -> [[a]]
windows n = transpose . take n . tails

part1 :: String -> Int
part1 = solve 4

part2 :: String -> Int
part2 = solve 14

solve :: Int -> String -> Int
solve n = (+n) . length . takeWhile (\s -> (length $ nub s) /= n) . windows n


main = do
    signal <- lines <$> readFile "day06.txt"
    mapM_ (print . part1) signal
    print "---"
    mapM_ (print . part2) signal