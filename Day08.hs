import Data.Char

part1 :: [[Int]] -> Int
part1 grid = length [() | x <- [0..size-1], y <- [0..size-1], visible x y]
    where size = length grid
          visible x y | x == 0 || y == 0 || x == size-1 || y == size -1 = True
          visible x y = shorter [grid !! y !! xx | xx <- [x,x-1..0]] ||
                        shorter [grid !! y !! xx | xx <- [x..size-1]] ||
                        shorter [grid !! yy !! x | yy <- [y,y-1..0]] ||
                        shorter [grid !! yy !! x | yy <- [y..size-1]]
          shorter (x:xs) = not $ any (>= x) xs

main = do
    grid <- map (map digitToInt) . lines <$> readFile "day08.txt"
    print $ part1 grid
