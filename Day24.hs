{-# LANGUAGE TupleSections #-}
import Data.List
import Data.Maybe
import Algorithm.Search

type Point = (Int, Int)
type Velocity = Point
type Blizzard = (Point, Velocity)
type Weather = [Blizzard]
type State = (Point, Int)

add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
dist (x1,y1) (x2,y2) = abs (x2-x1) + abs (y2-y1)
neighbours = [(0,0),(1,0),(-1,0),(0,1),(0,-1)]

next :: Int -> Int -> [Weather] -> State -> [State]
next w h weathers (pos, time) = map (,time+1) nextPos
    where nextPos = filter valid $ map (add pos) neighbours
          valid (x, 0) = pos == (x,0)
          valid (x, y) | y == h-1 = pos == (x,y)
          valid (x, y) = x >= 1 && x <= w-2 && y >= 1 && y <= h-2 && isNothing (lookup (x,y) $ weathers !! time)

radar :: [String] -> Weather
radar scan = [((x,y), wind c) | x <- [0..length (head scan) -1], y <- [0..length scan -1], let c = scan !! y !!x, c `elem` "><^v"]

wind :: Char -> Point
wind '>' = (1,0)
wind '<' = (-1,0)
wind '^' = (0,-1)
wind 'v' = (0,1)
wind _ = error "No such wind"

advance :: Int -> Int -> Blizzard -> Blizzard
advance w h (pos,vel) = ((wrap w x', wrap h y'), vel)
    where (x',y') = add pos vel
          wrap len n
            | n >= len-1 = 1
            | n < 1 = len-2
            | otherwise = n

main = do
    scan <- lines <$> readFile "day24.txt"
    let start = (fromJust $ elemIndex '.' $ head scan, 0)
        exit = (fromJust $ elemIndex '.' $ last scan, length scan - 1)
        goal = (fromJust $ elemIndex '.' $ last scan, length scan - 2)
        back = (fromJust $ elemIndex '.' $ head scan, 1)
        height = length scan
        width = length $ head scan
        weather = radar scan
        weathers = iterate (map $ advance width height) weather
        Just pathOut = aStar (next width height weathers) (const $ const 1) (dist goal.fst) ((==)goal.fst) (start, 0)
        Just pathBack = aStar (next width height weathers) (const $ const 1) (dist back.fst) ((==)back.fst) (exit, fst pathOut)
        Just pathAgain = aStar (next width height weathers) (const $ const 1) (dist goal.fst) ((==)goal.fst) (start, fst pathOut + fst pathBack)
    -- part 1
    print $ fst pathOut
    -- part 2
    print $ fst pathOut + fst pathBack + fst pathAgain
