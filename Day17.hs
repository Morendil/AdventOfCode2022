import qualified Data.Set as S

type Point = (Int, Int)
type Grid = S.Set Point
type Piece = ([Point],(Int,Int))
type Rock = (Int,Point)
type State = (Grid, String, Int)

pieces :: [[Point]]
pieces = [
    [(0,0),(1,0),(2,0),(3,0)],
    [(1,0),(0,1),(1,1),(2,1),(1,2)],
    [(0,0),(1,0),(2,0),(2,1),(2,2)],
    [(0,0),(0,1),(0,2),(0,3)],
    [(0,0),(1,0),(0,1),(1,1)]
    ]

add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

move :: State -> State
move state@(grid,moves,piece) = move' state (2,yMax+3)
    where yMax = if S.null grid then 0 else 1+(maximum $ S.map snd $ grid)

move' :: State -> Point -> State
move' state@(grid,moves,piece) pos = if stop then (grid', tail moves, nextPiece) else move' (grid, tail moves, piece) fell
    where nextPiece = (piece + 1) `mod` length pieces
          pushed = push state (head moves) pos
          (fell, stop) = fall state pushed
          grid' = foldl (flip S.insert) grid (map (add fell) (pieces !! piece))

push :: State -> Char -> Point -> Point
push (grid,_,piece) dir (x,y) = if any invalid candidates then (x,y) else newPos
    where effect = if dir == '<' then -1 else 1
          newPos = (x+effect,y)
          candidates = map (add newPos) (pieces !! piece)
          invalid (x,y) = x < 0 || x >= 7 || (x,y) `S.member` grid

fall :: State -> Point -> (Point, Bool)
fall (grid,_,piece) (x,y) = if any invalid candidates then ((x,y),True) else (newPos,False)
    where newPos = (x,y-1)
          candidates = map (add newPos) (pieces !! piece)
          invalid (x,y) = y < 0 || (x,y) `S.member` grid

display :: State -> [String]
display (grid,moves,_) = [[if (x,y) `S.member` grid then '#' else '.' | x <- [0..6]] | y <- reverse [0..yMax]]
    where yMax = if S.null grid then 0 else maximum $ S.map snd $ grid
          yMin = if S.null grid then 0 else minimum $ S.map snd $ grid

part1 :: String -> Int
part1 moves = yMax + 1
    where start = (S.empty, cycle moves, 0)
          n = 2022 + 1
          (grid,_,_) = last $ take n $ iterate move start
          yMax = maximum $ S.map snd $ grid

main = do
    moves <- readFile "day17.txt"
    print $ part1 moves
