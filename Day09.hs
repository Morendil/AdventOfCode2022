import Data.List (nub)
import Test.Hspec
import Test.QuickCheck

type Pair = (Int, Int)
type State = (Pair, Pair)
type LongRope = [Pair]

dragHead :: LongRope -> Char -> LongRope
dragHead rope dir = dragRope (newHead:tail rope)
    where newHead = move dir (head rope)

dragRope :: LongRope -> LongRope
dragRope (headKnot:follower:rest) = headKnot:dragRope (adjusted:rest)
    where adjusted = adjust follower headKnot
dragRope shorter = shorter

adjust :: Pair -> Pair -> Pair
adjust (ox,oy) (hx,hy) = (nx, ny)
    where (dx, dy) = (hx-ox, hy-oy)
          apart = abs dx > 1 || abs dy > 1
          nx = if apart && abs dx > 0 then ox + (dx `div` abs dx) else ox
          ny = if apart && abs dy > 0 then oy + (dy `div` abs dy) else oy

move :: Char -> Pair -> Pair
move 'R' (x,y) = (x+1,y)
move 'L' (x,y) = (x-1,y)
move 'U' (x,y) = (x,y+1)
move 'D' (x,y) = (x,y-1)
move c _ = error $ "Unknown direction " ++ [c]

tailPositions :: LongRope -> String -> [Pair]
tailPositions rope = map last . scanl dragHead rope

rope :: Int -> LongRope
rope n = replicate n (0,0)

solve :: Int -> String -> Int
solve n = length . nub . tailPositions (rope n)

part1 :: String -> Int
part1 = solve 2

part2 :: String -> Int
part2 = solve 10

unfold :: [String] -> String
unfold = concatMap (go . words)
    where go c@(cmd:n:_) = replicate (read n) (head cmd)
          go x = error "Can't interpret command " ++ concat x

main = do
    commands <- lines <$> readFile "day09.txt"
    print $ part1 $ unfold commands
    print $ part2 $ unfold commands

test :: IO ()
test = hspec $ do
  describe "Adjusting a knot" $ do
    it "Doesn't adjust tail if less than 1 away, horizontally" $
        adjust (0,0) (1,0) `shouldBe` (0,0)
    it "Doesn't adjust tail if less than 1 away, vertically" $
        adjust (0,0) (0,1) `shouldBe` (0,0)
    it "Adjusts tail so that it catches up, horizontally, forward" $
        adjust (0,0) (2,0) `shouldBe` (1,0)
    it "Adjusts tail so that it catches up, horizontally, back" $
        adjust (4,0) (2,0) `shouldBe` (3,0)
    it "Adjusts tail so that it catches up, vertically, forward" $
        adjust (0,0) (0,2) `shouldBe` (0,1)
    it "Adjusts tail so that it catches up, vertically, back" $
        adjust (0,4) (0,2) `shouldBe` (0,3)
    it "Doesn't adjust tail if in contact diagonally" $
        adjust (0,0) (1,1) `shouldBe` (0,0)
    it "Adjusts tail so that it catches up, diagonally, up" $
        adjust (0,0) (1,2) `shouldBe` (1,1)
    it "Adjusts tail so that it catches up, diagonally, right" $
        adjust (0,0) (2,1) `shouldBe` (1,1)
  describe "Moving multiple knots" $ do
    it "Preserves the length of a rope" $
      property (\rope -> length rope == length (dragRope rope))