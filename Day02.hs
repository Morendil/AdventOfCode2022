part1 :: [[String]] -> Int
part1 = sum . map score

part2 :: [[String]] -> Int
part2 = sum . map score2

-- Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock
-- A for Rock, B for Paper, and C for Scissors
-- X for Rock, Y for Paper, and Z for Scissors
-- 1 for Rock, 2 for Paper, and 3 for Scissors
-- 0 if you lost, 3 if the round was a draw, and 6 if you won
score :: [String] -> Int
score ("A":"X":_) = 3 + 1
score ("A":"Y":_) = 6 + 2
score ("A":"Z":_) = 0 + 3
score ("B":"X":_) = 0 + 1
score ("B":"Y":_) = 3 + 2
score ("B":"Z":_) = 6 + 3
score ("C":"X":_) = 6 + 1
score ("C":"Y":_) = 0 + 2
score ("C":"Z":_) = 3 + 3
score other = error $ concat other

-- Rock defeats Scissors, Scissors defeats Paper, and Paper defeats Rock
-- A for Rock, B for Paper, and C for Scissors
-- X means you need to lose, Y means you need to end the round in a draw, and Z means you need to win.
-- X for Rock, Y for Paper, and Z for Scissors
-- 1 for Rock, 2 for Paper, and 3 for Scissors
-- 0 if you lost, 3 if the round was a draw, and 6 if you won
score2 :: [String] -> Int
score2 ("A":"X":_) = 3 + 0
score2 ("A":"Y":_) = 1 + 3
score2 ("A":"Z":_) = 2 + 6
score2 ("B":"X":_) = 1 + 0
score2 ("B":"Y":_) = 2 + 3
score2 ("B":"Z":_) = 3 + 6
score2 ("C":"X":_) = 2 + 0
score2 ("C":"Y":_) = 3 + 3
score2 ("C":"Z":_) = 1 + 6
score2 other = error $ concat other

main = do
    rounds <- (map words . lines) <$> readFile "day02.txt"
    print $ part1 $ rounds
    print $ part2 $ rounds