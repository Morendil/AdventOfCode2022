import Data.Char (isNumber, isAlpha, isSpace)
import Data.Maybe
import Data.List
import Data.Tuple
import Data.Tuple.Extra ( first, second, both )
import Data.List.HT hiding (break)
import Test.Hspec
import Debug.Trace

type Direction = (Int, Int)
data Command = Move Int | Turn Char
    deriving (Eq, Show)
type State = (Direction, (Int, Int))
type Grid = [String]

type CubeState = (Direction, Int, (Int, Int))
type Face = (Int,Int,Int)

start grid = (fromJust $ elemIndex '.' (head grid), 0)
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
mul (x1,y1) (x2,y2) = (x1*x2,y1*y2)

-- Facing is 0 for right (>), 1 for down (v), 2 for left (<), and 3 for up (^).
dirScore (1,0) = 0
dirScore (0,1) = 1
dirScore (-1,0) = 2
dirScore (0,-1) = 3
dirScore _ = error "Bad direction"

score :: State -> Int
score (dir,(x,y)) = (1000 * (y+1)) + (4 * (x+1)) + dirScore dir

execute :: Grid -> [Command] -> State
execute grid = foldl (step grid) ((1,0), start grid)

turn :: (Int, Int) -> Char -> (Int, Int)
turn (1,0) 'L' = (0,-1)
turn (1,0) 'R' = (0,1)
turn (-1,0) 'L' = (0,1)
turn (-1,0) 'R' = (0,-1)
turn (0,1) 'L' = (1,0)
turn (0,1) 'R' = (-1,0)
turn (0,-1) 'L' = (-1,0)
turn (0,-1) 'R' = (1,0)
turn _ _ = error "Bad turn"

step :: Grid -> State -> Command -> State
step grid (dir, pos) (Move n) = wrapMove grid (dir, pos) pos (Move n)
step grid (dir, pos) (Turn c) = (turn dir c, pos)

wrapMove :: Grid -> State -> (Int, Int) -> Command -> State
wrapMove grid (dir, pos) prev (Move 0) = (dir, pos)
wrapMove grid (dir, pos) prev (Move n) = case grid !! wrapY !! wrapX of
                                        '#' -> (dir, prev)
                                        '.' -> wrapMove grid (dir, (wrapX,wrapY)) (wrapX,wrapY) $ Move (n-1)
                                        ' ' -> wrapMove grid (dir, (wrapX,wrapY)) prev $ Move n
                                        _ -> error "What's that ?"
    where (newX, newY) = add pos dir
          wrapY | newY < 0 = length grid - 1        | newY >= length grid = 0        | otherwise = newY
          wrapX | newX < 0 = length (head grid) - 1 | newX >= length (head grid) = 0 | otherwise = newX
wrapMove _ _ _ _ = error "Bad move"

cubeScore :: CubeState -> Int
cubeScore (dir,grid,(x,y)) = (1000 * (y+1)) + (4 * (x+1)) + dirScore dir

cubeExec :: [[Face]] -> [Grid] -> [Command] -> CubeState
cubeExec model grids = foldl (cubeStep model grids) ((1,0), 1, (0,0))

cubeStep :: [[Face]] -> [Grid] -> CubeState -> Command -> CubeState
cubeStep model grids (dir, grid, pos) (Turn c) = (turn dir c, grid, pos)
cubeStep model grids state (Move 0) = state
cubeStep model grids state@(dir, grid, pos) (Move n) = case grids !! (grid'-1) !! wrapY !! wrapX of
                                        '#' -> (dir, grid, pos)
                                        '.' -> cubeStep model grids state' $ Move (n-1)
                                        _ -> error "What's that ?"
    where state'@(_, grid', (wrapX, wrapY)) = cubeWrap (model !! (grid-1)) (length $ head grids) state 

cubeWrap :: [Face] -> Int -> CubeState -> CubeState
cubeWrap faces side (dir,index,pos) = (dir',index',rewrap s (wrapX, wrapY))
    where (newX, newY) = add pos dir
          wrapY        | newY < 0 = side - 1 | newY >= side = 0 | otherwise = newY
          wrapX        | newX < 0 = side - 1 | newX >= side = 0 | otherwise = newX
          (index',t,s) | newX < 0 = faces !! 2
                       | newX >= side = head faces
                       | newY < 0 = faces !! 3
                       | newY >= side = faces !! 1
                       | otherwise = (index,0,0)
          dir' = chg t dir
          chg 0 = id
          chg 1 = flip turn 'L'
          chg 2 = flip turn 'R'
          chg 3 = flip turn 'R' . flip turn 'R'
          chg _ = error "Bad turn"
          rewrap 0 = id
          rewrap 1 = first ((side-1) -)
          rewrap 2 = second ((side-1) -)
          rewrap 3 = both ((side-1) -)
          rewrap 4 = swap
          rewrap 5 = first ((side-1) -) . swap 
          rewrap 6 = second ((side-1) -) . swap
          rewrap 7 = both ((side-1) -) . swap
          rewrap _ = error "?"

part1 = do
    input <- lines <$> readFile "day22.txt"
    let (grid, _:[moves]) = break null input
        instructions = parseCommands moves
        grid' = map (padRight ' ' (maximum $ map length grid)) grid
    print $ score $ execute grid' instructions

part2 (w,h,cube,name) = do
    input <- lines <$> readFile name
    let (grid, _:[moves]) = break null input
        instructions = parseCommands moves
        grid' = map (padRight ' ' (maximum $ map length grid)) grid
        grids = filter (not . any (any isSpace))$ concatMap (transpose . map (nChunks w)) $ nChunks h grid'
    print $ cubeExec cube grids instructions

part2_sample :: (Int, Int, [[Face]], String)
part2_sample = (4,3,sampleCube,"day22_sample.txt")
part2_input :: (Int, Int, [[Face]], String)
part2_input = (3,4,inputCube,"day22.txt")

sampleCube :: [[Face]]
-- right, down, left, up
sampleCube = [
    [(6,3,3),(4,0,0),(3,1,6),(2,3,2)],
    [(3,0,0),(5,3,2),(6,2,5),(1,3,2)],
    [(4,0,0),(5,1,6),(2,0,0),(1,1,5)],
    [(6,2,5),(5,0,0),(3,0,0),(1,0,0)],
    [(6,0,0),(2,3,3),(3,2,5),(4,0,0)],
    [(1,3,3),(2,2,6),(5,0,0),(4,1,6)]]
 :: [[Face]]
-- right, down, left, up
inputCube :: [[Face]]
inputCube = []

parseCommands :: String -> [Command]
parseCommands [] = []
parseCommands (c:rest) | isAlpha c = Turn c:parseCommands rest
parseCommands num = move:parseRest
    where move = Move (read $ takeWhile isNumber num)
          parseRest = parseCommands $ dropWhile isNumber num

nChunks n l = chunksOf (length l `div` n) l
chunksOf n l | length l <= n = [l]
chunksOf n l = take n l : chunksOf n (drop n l)

test :: IO ()
test = hspec $ do
  describe "Cube wrapping" $ do
    it "going to an aligned face" $ do
        let aligned = [(2,0,0),(3,0,0),(4,0,0),(5,0,0)]
        cubeWrap aligned 8 ((1,0), 1, (7,1)) `shouldBe` ((1,0), 2, (0,1))
        cubeWrap aligned 8 ((0,1), 1, (1,7)) `shouldBe` ((0,1), 3, (1,0))
        cubeWrap aligned 8 ((-1,0), 1, (0,1)) `shouldBe` ((-1,0), 4, (7,1))
        cubeWrap aligned 8 ((0,-1), 1, (1,0)) `shouldBe` ((0,-1), 5, (1,7))
    it "going to non-aligned faces" $ do
        cubeWrap [(6,3,3),(99,0,0),(99,0,0),(99,0,0)] 8 ((1,0), 1, (7,1))  `shouldBe` ((-1,0), 6, (7,6))
        cubeWrap [(99,0,0),(99,0,0),(3,1,6),(99,0,0)] 8 ((-1,0), 1, (0,1)) `shouldBe` ((0,1), 3, (1,0))
        cubeWrap [(99,0,0),(99,0,0),(99,0,0),(2,3,2)] 8 ((0,-1), 1, (1,0)) `shouldBe` ((0,1), 2, (1,0))
        cubeWrap [(99,0,0),(99,0,0),(6,2,5),(99,0,0)] 8 ((-1,0), 2, (0,1)) `shouldBe` ((0,-1), 6, (6,7))
        cubeWrap [(99,0,0),(5,1,6),(99,0,0),(99,0,0)] 8 ((0,1), 3, (1,7))  `shouldBe` ((1,0), 5, (0,6))
        -- This is the one from the example
        cubeWrap [(6,2,5),(99,0,0),(99,0,0),(99,0,0)] 4 ((1,0), 4, (3,1))  `shouldBe` ((0,1), 6, (2,0))
