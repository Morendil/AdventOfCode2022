unfold :: [String] -> [Int]
unfold = reverse . foldl expand [1]
    where expand (last:rest) "noop" = last:last:rest
          expand (last:rest) addx = (read $ drop 5 addx)+last:last:last:rest

part1 :: [String] -> Int
part1 instructions = sum $ map (\n -> n * (xvalues !! (n-1))) [20, 60, 100, 140, 180, 220]
    where xvalues = unfold instructions

main = do
    instructions <- lines <$> readFile "day10.txt"
    print $ part1 instructions
