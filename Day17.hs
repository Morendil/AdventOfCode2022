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
move state@(grid,moves,piece) = tetris
    where yMax = if S.null grid then 0 else 1+(maximum $ S.map snd $ grid)
          yMin = if S.null grid then 0 else 1+(minimum $ S.map snd $ grid)
          tetris = if null tetrisLines then state' else cleanup
          tetrisLines = filter (\y -> all (\x -> (x,y) `S.member` grid') [0..6]) [yMin..yMax]
          cleanup = (S.filter (((<=)(maximum tetrisLines)).snd) grid', move', piece')
          state'@(grid',move',piece') = settle state (2,yMax+3)

settle :: State -> Point -> State
settle state@(grid,moves,piece) pos = if stop then (grid', tail moves, nextPiece) else settle (grid, tail moves, piece) fell
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
display (grid,moves,_) = [[if (x,y) `S.member` grid then '#' else '.' | x <- [0..6]] | y <- reverse [yMin..yMax]]
    where yMax = if S.null grid then 0 else maximum $ S.map snd $ grid
          yMin = if S.null grid then 0 else minimum $ S.map snd $ grid

part1 :: String -> Int
part1 moves = height 2022 moves

height :: Int -> String -> Int
height n moves = yMax + 1
    where start = (S.empty, cycle moves, 0)
          (grid,_,_) = last $ take (n+1) $ iterate move start
          yMax = maximum $ S.map snd $ grid

topAfter :: Int -> String -> [String]
topAfter n moves = display $ (S.filter (((<)(yMax-10)).snd) grid,"",0)
    where start = (S.empty, cycle moves, 0)
          s@(grid,_,_) = last $ take (n+1) $ iterate move start
          yMax = maximum $ S.map snd $ grid

top :: State -> String
top (grid,_,_) = concat $ display $ (S.filter (((<)(yMax-10)).snd) grid,"",0)
    where yMax = if S.null grid then 0 else maximum $ S.map snd $ grid

debug n = putStrLn $ unlines $ topAfter n ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

part2 :: String -> Int
part2 moves = cycleHeight * (1000000000000 `div` period) + height (1000000000000 `mod` period) moves
    where (init,loop) = findCycle $ map top $ states
          initHeight = height front moves
          cycleHeight = height (front + period) moves - initHeight
          states = filter (\(_,_,p) -> p `mod` 5 == 0) $ iterate move (S.empty, cycle moves, 0)
          period = length loop * 5
          front = length init * 5

findCycle :: Eq a => [a] -> ([a],[a])
findCycle xxs = fCycle xxs xxs
  where fCycle (x:xs) (_:y:ys)
         | x == y              = fStart xxs xs
         | otherwise           = fCycle xs ys
        fCycle _      _        = (xxs,[]) -- not cyclic
        fStart (x:xs) (y:ys)
         | x == y              = ([], x:fLength x xs)
         | otherwise           = let (as,bs) = fStart xs ys in (x:as,bs)
        fLength x (y:ys)
         | x == y              = []
         | otherwise           = y:fLength x ys

main = do
    moves <- readFile "day17.txt"
    print $ part1 moves
    print $ part2 moves
