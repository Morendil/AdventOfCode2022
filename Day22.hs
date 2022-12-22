import Data.Char (isNumber, isAlpha)
import Data.Maybe
import Data.List
import Data.List.HT hiding (break)

data Direction = GoRight | GoDown | GoLeft | GoUp
    deriving (Eq, Show)
data Command = Move Int | Turn Char
    deriving (Eq, Show)
type State = (Direction, (Int, Int))
type Grid = [String]

start grid = (fromJust $ elemIndex '.' (head grid), 0)
offsets = [(GoRight,(1,0)),(GoUp,(0,-1)),(GoDown,(0,1)),(GoLeft,(-1,0))]
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

-- Facing is 0 for right (>), 1 for down (v), 2 for left (<), and 3 for up (^).
dirScore GoRight = 0
dirScore GoDown = 1
dirScore GoLeft = 2
dirScore GoUp = 3

score :: State -> Int
score (dir,(x,y)) = (1000 * (y+1)) + (4 * (x+1)) + dirScore dir

execute :: Grid -> [Command] -> State
execute grid = foldl (step grid) (GoRight, start grid)

step :: Grid -> State -> Command -> State
step grid (GoRight, pos) (Turn 'L') = (GoUp, pos)
step grid (GoRight, pos) (Turn 'R') = (GoDown, pos)
step grid (GoLeft, pos) (Turn 'L') = (GoDown, pos)
step grid (GoLeft, pos) (Turn 'R') = (GoUp, pos)
step grid (GoDown, pos) (Turn 'L') = (GoRight, pos)
step grid (GoDown, pos) (Turn 'R') = (GoLeft, pos)
step grid (GoUp, pos) (Turn 'L') = (GoLeft, pos)
step grid (GoUp, pos) (Turn 'R') = (GoRight, pos)
step grid (dir, pos) (Move n) = wrapMove grid (dir, pos) pos (Move n)
step _ _ _ = error "Bad step"

wrapMove :: Grid -> State -> (Int, Int) -> Command -> State
wrapMove grid (dir, pos) prev (Move 0) = (dir, pos)
wrapMove grid (dir, pos) prev (Move n) = case grid !! wrapY !! wrapX of
                                        '#' -> (dir, prev)
                                        '.' -> wrapMove grid (dir, (wrapX,wrapY)) (wrapX,wrapY) $ Move (n-1)
                                        ' ' -> wrapMove grid (dir, (wrapX,wrapY)) prev $ Move n
                                        _ -> error "What's that ?"
    where (newX, newY) = add pos $ fromJust $ lookup dir offsets
          wrapY | newY < 0 = length grid - 1        | newY >= length grid = 0        | otherwise = newY
          wrapX | newX < 0 = length (head grid) - 1 | newX >= length (head grid) = 0 | otherwise = newX
wrapMove _ _ _ _ = error "Bad move"

part1 = do
    input <- lines <$> readFile "day22.txt"
    let (grid, _:[moves]) = break null input
        instructions = parseCommands moves
        grid' = map (padRight ' ' (maximum $ map length grid)) grid
    print $ score $ execute grid' instructions

parseCommands :: String -> [Command]
parseCommands [] = []
parseCommands (c:rest) | isAlpha c = Turn c:parseCommands rest
parseCommands num = move:parseRest
    where move = Move (read $ takeWhile isNumber num)
          parseRest = parseCommands $ dropWhile isNumber num
