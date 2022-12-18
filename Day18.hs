import qualified Data.Text as T
import qualified Data.Set as S

type Cube = (Int, Int, Int)

part1 :: [Cube] -> Int
part1 cubes = sum $ map faces cubes
    where cubeSet = S.fromList cubes
          occluded c = length $ filter (flip S.member cubeSet) $ map (add c) neighbours
          faces c = 6 - occluded c

add (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)
neighbours = [(1,0,0),(-1,0,0),(0,1,0),(0,-1,0),(0,0,1),(0,0,1)]
triplet (x:y:z:_) = (x,y,z)
triplet _ = error "Not a triplet"

main = do
    cubes <- map (triplet . map read . map T.unpack . T.splitOn (T.pack ",") . T.pack) . lines <$> readFile "day18.txt"
    print $ part1 cubes