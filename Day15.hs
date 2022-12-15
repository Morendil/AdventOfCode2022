import Text.ParserCombinators.ReadP
import Data.Maybe (fromJust, mapMaybe)
import Data.Char (isNumber, chr)
import Data.List (nub, sort, sortOn)
import Test.Hspec

type Point = (Int, Int)
type Range = (Int, Int)
type Circle = (Point, Int)

intersect :: Int -> (Point, Point) -> Maybe (Range)
intersect row (sensor@(sx,sy), beacon@(bx,by)) = if vert > r then Nothing else Just (sx-(r-vert),sx+(r-vert))
    where r = radius (sensor, beacon)
          vert = abs (sy-row)

circle :: (Point, Point) -> Circle
circle (s,b) = (s, radius (s,b))

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
part1 row sensors = (sum $ map len $ zones row sensors) - beaconsOnRow
    where len (rmin,rmax) = 1 + abs (rmax-rmin)
          -- bit of a fudge (we should test for inclusion in the range maybe) but works on my input
          beaconsOnRow = length $ nub $ [(bx,by)|(_,(bx,by))<-sensors, by==row]

part2 :: Int -> [(Point,Point)] -> Int
part2 side sensors = frequency $ head candidates
    where candidates = uncovered side $ map circle sensors
          frequency (x,y) = (4000000*x)+y

zones ::  Int -> [(Point,Point)] -> [Range]
zones row sensors = converge (foldl insert []) $ mapMaybe (intersect row) $ sensors

cross :: Circle -> Circle -> [Point]
cross c1 c2 = [((b-a)`div`2,(a+b)`div`2)| a <- as c1 ++ as c2, b <- bs c1 ++ bs c2]
    where as ((sx,sy),r)=[sy-sx+r+1,sy-sx-r-1]
          bs ((sx,sy),r)=[sy+sx+r+1,sy+sx-r+1]

cover :: Point -> Circle -> Bool
cover (px,py) ((sx,sy),r) = (abs (px-sx) + abs (py-sy)) <= r

uncovered :: Int -> [Circle] -> [Point]
uncovered side circles = filter (\p -> empty p && inSquare p) $ nub $ concat [cross c1 c2 | c1 <- circles, c2 <- circles]
    where empty pt = not $ any (cover pt) circles
          inSquare (x,y) = x >= 0 && x <= side && y >= 0 && y <= side

isolators :: [Circle] -> [Circle]
isolators circles = nub $ concat [ [c1, c2] | c1@(s1,r1) <- circles, c2@(s2,r2) <-circles, radius (s1,s2) - (r1 + r2) == 2]

main = do
    -- let (dataFile, row, side) = ("day15_sample.txt", 10, 20)
    let (dataFile, row, side) = ("day15.txt", 2000000, 4000000)
    sensors <- fromJust . parseMaybe (sepBy1 sensorPair (string "\n")) <$> readFile dataFile
    print $ part1 row sensors
    print $ part2 side sensors

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

display :: Int -> Int -> [Circle] -> [String]
display xMax yMax circles = [ [ rep (x,y) circles | x <- [0..xMax]] | y <- [0..yMax]]
  where rep pt circles = if (null covering) then '.' else color $ head covering
                            where covering = filter (\((center,r),c) -> radius (center,pt) <= r) colored
                                  colored = zip circles [65..]
                                  color (_,c) = chr c

displayCrossings :: Int -> Int -> [Circle] -> [String]
displayCrossings xMax yMax circles = [ [ rep (x,y) circles | x <- [0..xMax]] | y <- [0..yMax]]
  where rep pt@(x,y) circles = if (null covering) then '.' else color $ head covering
                            where covering = filter borders colored
                                  borders (((sx,sy),r),c) | y ==  x + sy-sx+r+1 = True
                                  borders (((sx,sy),r),c) | y ==  x + sy-sx-r-1 = True
                                  borders (((sx,sy),r),c) | y == -x + sy+sx+r+1 = True
                                  borders (((sx,sy),r),c) | y == -x + sy+sx-r-1 = True
                                  borders _ = False
                                  colored = zip circles [65..]
                                  color (_,c) = chr c


test :: IO ()
test = hspec $ do
  describe "Beacons" $ do
    it "intersecting with a row" $
        intersect 10 ((8,7),(2,10)) `shouldBe` Just (2,14)
    it "intersecting with a row at single position" $
        intersect 16 ((8,7),(2,10)) `shouldBe` Just (8,8)
    it "intersecting with a row nowhere" $
        intersect 17 ((8,7),(2,10)) `shouldBe` Nothing
