unfold :: [String] -> [Int]
unfold = reverse . foldl expand [1]
    where expand (last:rest) "noop" = last:last:rest
          expand (last:rest) addx = (read $ drop 5 addx)+last:last:last:rest

part1 :: [String] -> Int
part1 instructions = sum $ map (\n -> signal !! (n-1)) [20, 60, 100, 140, 180, 220]
    where xvalues = unfold instructions
          signal = zipWith (*) xvalues [1..]

part2 :: [String] -> [String]
part2 instructions = [[pixel x y | x <- [0..39]] | y <- [0..5]]
    where xvalues = unfold instructions
          sprite x y = xvalues !! ((y*40)+x)
          pixel x y = if x == sprite x y - 1 || x == sprite x y || x == sprite x y + 1 then '#' else '.'

main = do
    instructions <- lines <$> readFile "day10.txt"
    print $ part1 instructions
    mapM_ putStrLn $ part2 instructions
