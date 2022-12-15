import Text.ParserCombinators.ReadP
import Data.Maybe (fromJust, mapMaybe)
import Data.Char (isNumber)
import Data.List (nub, sort)
import Test.Hspec

type Point = (Int, Int)
type Range = (Int, Int)

intersect :: Int -> (Point, Point) -> Maybe (Range)
intersect row (sensor@(sx,sy), beacon@(bx,by)) = if vert > r then Nothing else Just (sx-(r-vert),sx+(r-vert))
    where r = radius (sensor, beacon)
          vert = abs (sy-row)

radius :: (Point, Point) -> Int
radius (sensor@(sx,sy), beacon@(bx,by)) = abs (sx-bx) + abs (sy-by)

merge :: Range -> Range -> [Range]
merge a@(amin,amax) b@(bmin,bmax) = if overlap then [(min amin bmin,max amax bmax)] else [a,b]
    where overlap = max amin bmin <= min amax bmax

insert :: [Range] -> Range -> [Range]
insert [] r = [r]
insert ranges r = sort $ nub $ concatMap (merge r) ranges

converge :: Eq a => (a -> a) -> a -> a
converge = until =<< ((==) =<<)

part1 :: Int -> [(Point,Point)] -> Int
part1 row sensors = (sum $ map len zones) - beaconsOnRow
    where zones = converge (foldl insert []) $ mapMaybe (intersect row) $ sensors
          len (rmin,rmax) = 1 + abs (rmax-rmin)
          -- bit of a fudge (we should test for inclusion in the range maybe) but works on my input
          beaconsOnRow = length $ nub $ [(bx,by)|(_,(bx,by))<-sensors, by==row]

main = do
    sensors <- fromJust . parseMaybe (sepBy1 sensorPair (string "\n")) <$> readFile "day15.txt"
    print $ part1 2000000 sensors

sensorPair = do
    sensor <- (,) <$> (string "Sensor at x=" *> number) <*> (string ", y=" *> number)
    beacon <- (,) <$> (string ": closest beacon is at x=" *> number) <*> (string ", y=" *> number)
    return (sensor,beacon)

number :: ReadP Int
number = do
    sign <- option 1 (string "-" *> pure (-1))
    mag <- read <$> many1 (satisfy isNumber)
    return $ sign * mag

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result

test :: IO ()
test = hspec $ do
  describe "Beacons" $ do
    it "intersecting with a row" $
        intersect 10 ((8,7),(2,10)) `shouldBe` Just (2,14)
    it "intersecting with a row at single position" $
        intersect 16 ((8,7),(2,10)) `shouldBe` Just (8,8)
    it "intersecting with a row nowhere" $
        intersect 17 ((8,7),(2,10)) `shouldBe` Nothing
