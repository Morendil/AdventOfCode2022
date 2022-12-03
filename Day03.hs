import Data.List (intersect)
import Data.Char (ord)
import Data.List.Split (chunksOf)

misplaced :: String -> Char
misplaced sack = head $ firstHalf `intersect` lastHalf
    where half = length sack `div` 2
          firstHalf = take half sack
          lastHalf = drop half sack

priority :: Char -> Int
priority c | c `elem` ['a'..'z'] = ord c - 96
priority c | c `elem` ['A'..'Z'] = ord c - 38
priority _ = 0

part1 :: [String] -> Int
part1 = sum . map (priority . misplaced)

common :: [String] -> Char
common = head . foldr1 intersect

part2 :: [String] -> Int
part2 = sum . map (priority . common) . chunksOf 3

main = do
    sacks <- lines <$> readFile "day03.txt"
    print $ part1 sacks
    print $ part2 sacks
