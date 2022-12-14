import Text.ParserCombinators.ReadP
import Data.Maybe (fromJust)
import Data.Char (isNumber)
import Data.Semigroup
import Data.List.HT (takeUntil)

import qualified Data.Map as M
import qualified Data.Ix as I

import Test.Hspec

type Point = (Int, Int)
type Scan = M.Map Point Char
data State = State Scan Bool

scanVein :: (Point, Point) -> Scan
scanVein ((xbeg,ybeg), (xend,yend)) | xbeg == xend = foldr (flip M.insert $ '#') M.empty [(xbeg,y)|y<-[min ybeg yend..max ybeg yend]]
scanVein ((xbeg,ybeg), (xend,yend)) | ybeg == yend = foldr (flip M.insert $ '#') M.empty [(x,ybeg)|x<-[min xbeg xend..max xbeg xend]]
scanVein _ = error "Not a straight line"

scanAll :: [[Point]] -> Scan
scanAll = foldMap (foldMap scanVein . pairs)
    where pairs l = zip l (tail l)

step :: Int -> State -> State
step yMax (State scan done) = State scan' isDone
  where scan' = if isDone then scan else M.insert (xrest,yrest) 'o' scan
        isDone = yrest >= yMax
        (xrest,yrest) = fallDown yMax scan (500,0)

fallDown :: Int -> Scan -> Point -> Point
fallDown yMax scan pt = if done then reached else fallDown yMax scan reached
    where (reached, done) = fall yMax scan pt

fall :: Int -> Scan -> Point -> (Point, Bool)
fall yMax scan (sx,sy) = (reached, done)
  where reached
          | canDropLeft = left
          | canDropRight = right
          | otherwise = bottom
        done = by > yMax || (not canDropLeft && not canDropRight)
        bottom@(bx,by) = last $ takeWhile (\(x,y) -> M.findWithDefault '.' (x,y) scan == '.') [(sx,y)| y<-[sy..yMax]]
        left = (bx-1, by+1)
        canDropLeft = supported && M.findWithDefault '.' left scan == '.'
        right = (bx+1, by+1)
        canDropRight = supported && M.findWithDefault '.' right scan == '.'
        supported = M.findWithDefault '.' (bx,by+1) scan /= '.'

main = do
    veins <- fromJust . parseMaybe veins <$> readFile "day14_sample.txt"
    let scan = scanAll veins
        initial = State scan False
        (_,(_,yMax)) = bounds $ M.keys scan
        isDone (State _ done) = done
        getScan (State scan _) = scan
        states = takeUntil isDone $ iterate (step yMax) initial
    -- part1
    print $ length states - 2
    putStrLn $ unlines $ display $ getScan $ last states

-- Parsing

number :: ReadP Int
number = read <$> many1 (satisfy isNumber)

veins = sepBy1 vein (string "\n")
vein = sepBy1 coord (string " -> ")
coord = (,) <$> number <*> (string "," *> number)

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

-- Debug

display :: Scan -> [String]
display scan = [ [ rep $ M.lookup (x,y) scan | x <- [xMin-1..xMax+1]] | y <- [yMin-1..yMax+1]]
  where rep (Just c) = c
        rep Nothing = '.'
        ((xMin,yMin),(xMax,yMax)) = bounds $ M.keys scan

bounds :: [Point] -> (Point, Point)
bounds points = ((xMin,yMin),(xMax,yMax))
  where (Min xMin, Min yMin, Max xMax, Max yMax) = foldMap (\(x,y) -> (Min x, Min y, Max x, Max y)) points

test :: IO ()
test = hspec $ do
  describe "Falling sand, in the lab" $ do
    let veins = [[(500,8),(500,8)],[(509,8),(510,8)],[(519,8),(521,8)]]
        scan = scanAll veins
    it "One grain falls into the abyss" $
        fall 10 scan (490,0) `shouldBe` ((490,10),True)
    it "One grain falls down to a support, then to the left" $
        fall 10 scan (500,0) `shouldBe` ((499,8),False)
    it "One grain falls down to a support, then to the right" $
        fall 10 scan (510,0) `shouldBe` ((511,8),False)
    it "One grain falls down to a support, coming to a rest" $
        fall 10 scan (520,0) `shouldBe` ((520,7),True)
  describe "Falling sand, in the sample" $ do
    let veins = [[(498,4),(498,6),(496,6)],[(503,4),(502,4),(502,9),(494,9)]]
        scan = scanAll veins
    it "One grain falls properly" $
        fallDown 10 scan (500,0) `shouldBe` (500,8)
