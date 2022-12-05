import Data.List (transpose)
import Data.Char (isAlpha, isLetter)
import Data.Array
import Data.Foldable (toList)
import Test.Hspec

test :: IO ()
test = hspec $ do
  describe "Moving crates" $ do
    it "does a single move" $
        doMove ["NZ","DCM","P"] (2,1) `shouldBe` ["DNZ","CM","P"]
    it "does multiple moves" $
        doMoves ["DNZ","CM","P"] [3,1,3] `shouldBe` ["","CM","ZNDP"]
    it "solves part 1" $
        part1 ["NZ","DCM","P"] [[1,2,1],[3,1,3],[2,2,1],[1,1,2]] `shouldBe` "CMZ"
    it "solves part 2" $
        part2 ["NZ","DCM","P"] [[1,2,1],[3,1,3],[2,2,1],[1,1,2]] `shouldBe` "MCD"

doMove :: [String] -> (Int, Int) -> [String]
doMove stacks (from, to) = toList $ arr // [(from, tail $ arr ! from),(to, (head $ arr ! from) : arr ! to)]
    where arr = listArray (1, length stacks) stacks

doMove2 :: [String] -> [Int] -> [String]
doMove2 stacks (n:from:to:_) = toList $ arr // [(from, drop n $ arr ! from),(to, (take n $ arr ! from) ++ arr ! to)]
    where arr = listArray (1, length stacks) stacks

doMoves :: [String] -> [Int] -> [String]
doMoves stacks (n:from:to:_) = foldl doMove stacks $ replicate n (from, to)

part1 :: [String] -> [[Int]] -> String
part1 stacks moves = map head $ foldl doMoves stacks moves

part2 :: [String] -> [[Int]] -> String
part2 stacks moves = map head $ foldl doMove2 stacks moves

main = do
    input <- lines <$> readFile "day05.txt"
    let (stackPicture, (_:moveDescriptions)) = break null input
    let stacks = map (reverse.filter isAlpha.tail.reverse) $ filter (any isAlpha) $ transpose stackPicture
    let moves :: [[Int]]
        moves = map (map read.words.filter (not.isLetter)) moveDescriptions
    print $ part1 stacks moves
    print $ part2 stacks moves
