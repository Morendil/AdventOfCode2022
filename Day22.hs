import Data.Char (isNumber, isAlpha)
import Data.List ( elemIndex )
import Data.List.HT ( padRight )
import Data.Tuple.Extra ( swap )
import Data.Maybe ( fromJust, isJust )
import Debug.Trace

data Direction = GoRight | GoDown | GoLeft | GoUp
    deriving (Eq, Show)
data Command = Move Int | Turn Char
    deriving (Eq, Show)
type State = (Direction, (Int, Int))
type Grid = [String]

type MoveFn = Grid -> State -> State -> Command -> State

start grid = (fromJust $ elemIndex '.' (head grid), 0)
offsets = [(GoRight,(1,0)),(GoUp,(0,-1)),(GoDown,(0,1)),(GoLeft,(-1,0))]
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
sub (x1,y1) (x2,y2) = (x1-x2,y1-y2)

-- Facing is 0 for right (>), 1 for down (v), 2 for left (<), and 3 for up (^).
dirScore GoRight = 0
dirScore GoDown = 1
dirScore GoLeft = 2
dirScore GoUp = 3

score :: State -> Int
score (dir,(x,y)) = (1000 * (y+1)) + (4 * (x+1)) + dirScore dir

execute :: MoveFn -> Grid -> [Command] -> State
execute move grid = foldl (step move grid) (GoRight, start grid)

step :: MoveFn -> Grid -> State -> Command -> State
step _ grid (GoRight, pos) (Turn 'L') = (GoUp, pos)
step _ grid (GoRight, pos) (Turn 'R') = (GoDown, pos)
step _ grid (GoLeft, pos) (Turn 'L') = (GoDown, pos)
step _ grid (GoLeft, pos) (Turn 'R') = (GoUp, pos)
step _ grid (GoDown, pos) (Turn 'L') = (GoRight, pos)
step _ grid (GoDown, pos) (Turn 'R') = (GoLeft, pos)
step _ grid (GoUp, pos) (Turn 'L') = (GoLeft, pos)
step _ grid (GoUp, pos) (Turn 'R') = (GoRight, pos)
step wrapMove grid (dir, pos) (Move n) = wrapMove grid (dir, pos) (dir,pos) (Move n)
step _ _ _ _ = error "Bad step"

wrapMove :: Grid -> State -> State -> Command -> State
wrapMove grid (dir, pos) prev (Move 0) = (dir, pos)
wrapMove grid (dir, pos) prev (Move n) = case grid !! wrapY !! wrapX of
                                        '#' -> prev
                                        '.' -> wrapMove grid next next $ Move (n-1)
                                        ' ' -> wrapMove grid (dir, (wrapX,wrapY)) prev $ Move n
                                        _ -> error "What's that ?"
    where (newX, newY) = add pos $ fromJust $ lookup dir offsets
          wrapY | newY < 0 = length grid - 1        | newY >= length grid = 0        | otherwise = newY
          wrapX | newX < 0 = length (head grid) - 1 | newX >= length (head grid) = 0 | otherwise = newX
          next = (dir, (wrapX,wrapY))
wrapMove _ _ _ _ = error "Bad move"

wrapMoveCube :: Teleporter -> Grid -> State -> State -> Command -> State
wrapMoveCube _ grid (dir, pos) prev (Move 0) = (dir, pos)
wrapMoveCube porter grid (dir, pos) prev@(_,oldPos) (Move n) = case grid !! nextY !! nextX of
                                        '#' -> prev
                                        '.' -> wrapMoveCube porter grid next next $ Move (n-1)
                                        ' ' -> if dest == '#' then prev else wrapMoveCube porter grid port port $ Move (n-1)
                                        _ -> error "What's that ?"
    where (newX, newY) = add pos $ fromJust $ lookup dir offsets
          next@(dir',(nextX, nextY)) = if wrap then porter pos (dir,(newX, newY)) else (dir,(newX,newY))
          wrap = newY < 0 || newY >= length grid || newX < 0 || newX >= length (head grid)
          port@(pDir,(portX,portY)) = porter oldPos next -- traceShow ("porter " ++ show oldPos++" "++show next++"->"++show (porter oldPos next)) $ porter oldPos next
          dest = grid !! portY !! portX

wrapMoveCube _ _ _ _ _ = error "Bad move"

type Teleporter = (Int,Int) -> (Direction, (Int, Int)) -> (Direction, (Int, Int))
samplePorter :: Teleporter
samplePorter (fx,fy) (dir,(x,y)) = case (fx`div`4,fy`div`4,dir) of
    -- cell 3
    (1,1,GoUp) -> (GoRight, ((2*4),3-x')) -- to 1
    -- cell 4
    (2,1,GoRight) -> (GoDown, ((3*4)+3-y', (2*4))) -- to 6
    -- cell 5
    (2,2,GoDown) -> (GoUp, (3-x', (1*4)+3)) -- to 6
    _ -> error $ "Into the void - "++show ((fx`div`4,fy`div`4,dir))
    where x'=x`mod`4
          y'=y`mod`4

inputPorter :: Teleporter
inputPorter (fx,fy) (dir,(x,y)) = case (fx`div`side,fy`div`side,dir) of
    -- cell 1
    (1,0,GoUp) -> (GoRight, (0*side, (3*side)+x')) -- to 6
    (1,0,GoLeft) -> (GoRight, (0*side, (3*side)-1-y')) -- to 4
    -- cell 2
    (2,0,GoRight) -> (GoLeft,(2*side-1,3*side-1-y')) -- to 5
    (2,0,GoDown) -> (GoLeft,(2*side-1,1*side+x')) -- to 3
    (2,0,GoUp) -> (GoUp, (0*side+x',4*side-1)) -- to 6
    -- cell 3
    (1,1,GoRight) -> (GoUp,(2*side+y',1*side-1)) -- to 2
    (1,1,GoLeft) -> (GoDown,(y',2*side)) -- to 4
    -- cell 4
    (0,2,GoLeft) -> (GoRight,(1*side,1*side-1-y')) -- to 1
    (0,2,GoUp) -> (GoRight,(1*side,1*side+x')) -- to 3
    -- cell 5
    (1,2,GoRight) -> (GoLeft,(3*side-1,1*side-1-y')) -- to 2
    (1,2,GoDown) -> (GoLeft,(1*side-1,3*side+x')) -- to 6
    -- cell 6
    (0,3,GoLeft) -> (GoDown, ((1*side)+y',0)) -- to 1
    (0,3,GoDown) -> (GoDown, (2*side+x',0)) -- to 2
    (0,3,GoRight) -> (GoUp,(1*side+y',3*side-1)) -- to 5
    _ -> error $ "Into the void - "++show ((fx`div`side,fy`div`side,dir))
    where x'=x`mod`side
          y'=y`mod`side
          side=50

part1 = do
    input <- lines <$> readFile "day22.txt"
    let (grid, _:[moves]) = break null input
        instructions = parseCommands moves
        grid' = map (padRight ' ' (maximum $ map length grid)) grid
    print $ score $ execute wrapMove grid' instructions

part2 :: (Teleporter, Int, FilePath) -> IO ()
part2 (cubePorter, side, fileName) = do
    input <- lines <$> readFile fileName
    let (grid, _:[moves]) = break null input
        instructions = parseCommands moves
        grid' = map (padRight ' ' (maximum $ map length grid)) grid
    print $ score $ execute (wrapMoveCube cubePorter) grid' instructions
    -- putStrLn $ unlines $ [[let c=lookup (x,y) turns in maybe (grid' !! y !! x) display c | x <- [0..(3*side)-1]] | y <- [0..(4*side)-1]]

display GoRight = '>'
display GoLeft = '<'
display GoDown = 'v'
display GoUp = '^'

parseCommands :: String -> [Command]
parseCommands [] = []
parseCommands (c:rest) | isAlpha c = Turn c:parseCommands rest
parseCommands num = move:parseRest
    where move = Move (read $ takeWhile isNumber num)
          parseRest = parseCommands $ dropWhile isNumber num