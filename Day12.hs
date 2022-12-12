import Data.Graph.AStar
import Data.Char
import Data.Maybe
import qualified Data.HashSet as H

add (x1,y1) (x2,y2) = (x1+x2,y1+y2)
neighbours pt = map (add pt) [(-1,0),(1,0),(0,1),(0,-1)]

at grid (x,y) | y < 0 = Nothing
at grid (x,y) | y >= length grid = Nothing
at grid (x,y) | x < 0 = Nothing
at grid (x,y) | x >= length (head grid) = Nothing
at grid (x,y) = Just $ (grid !! y) !! x

manhattan (x1,y1) (x2,y2) = abs (y2-y1) + abs (x2-x1)

main = do
    grid <- lines <$> readFile "day12.txt"
    let height = length grid - 1
        width = length (head grid) - 1
        reachable pt Nothing = False
        reachable pt (Just 'E') = (fromJust $ at grid pt) >= 'y'
        reachable pt _ | pt == start = True
        reachable pt (Just c) = (succ $ fromJust $ at grid pt) >= c
        around pt = H.fromList $ filter (reachable pt . at grid) (neighbours pt)
        start = head [(x,y)| x<-[0..width], y<-[0..height],at grid (x,y)==Just 'S']
        goal = head [(x,y)| x<-[0..width], y<-[0..height],at grid (x,y)==Just 'E']
    -- part1
    print $ length $ fromJust $ aStar around (\from to -> 1) (manhattan start) (== goal) start
    -- part2
    let candidates = [(x,y)| x<-[0..width], y<-[0..height],at grid (x,y)==Just 'S' || at grid (x,y)==Just 'a']
        paths = map (\start -> aStar around (\from to -> 1) (manhattan start) (== goal) start) candidates
    print $ minimum $ map length $ catMaybes paths
