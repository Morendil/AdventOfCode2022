import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.List.HT
import Data.List.Extra
import Data.Maybe
import Data.Semigroup

type Point = (Int, Int)
type Votes = M.Map (Int,Int) [(Int,Int)]
type Rule = ((Int,Int),[(Int,Int)])
type State = (Int, S.Set (Int, Int), Bool)

locations :: [String] -> S.Set (Int,Int)
locations scan = S.fromList $ concat [[(x,y) | x <- elemIndices '#' $ scan !! y]| y <- [0..length scan -1]]
add (x1,y1) (x2,y2) = (x1+x2,y1+y2)

neighbours = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
quadrants = [
    ((0,-1),[(0,-1),(-1,-1),(1,-1)]),
    ((0,1),[(0,1),(-1,1),(1,1)]),
    ((-1,0),[(-1,0),(-1,1),(-1,-1)]),
    ((1,0),[(1,0),(1,-1),(1,1)])]

propose :: S.Set (Int, Int) -> [Rule] -> (Int, Int) -> Maybe (Int, Int)
propose elves rules pos = firstJust id candidates
    where try (dest, spots) = if any (occupied . add pos) spots then Nothing else Just (add dest pos)
          occupied spot = spot `S.member` elves
          candidates = map try rules

alone :: S.Set (Int, Int) -> (Int, Int) -> Bool
alone elves elf = not $ any (occupied . add elf) neighbours 
    where occupied spot = spot `S.member` elves

doVote :: [Rule] -> S.Set (Int, Int) -> Votes -> (Int, Int) -> Votes
doVote rules elves votes elf = case propose elves rules elf of
    Nothing -> votes
    Just vote -> M.alter (Just . maybe [elf] (elf:)) vote votes

doRound :: State -> State
doRound (time, elves, _) = (time+1, doMoves, settled)
    where (lone, crowded) = S.partition (alone elves) elves
          settled = S.size lone == S.size elves
          rules = take 4 $ drop (time `mod` 4) $ cycle quadrants
          votes = S.foldl (doVote rules elves) M.empty crowded
          unique = M.filter ((==)1.length) votes
          doMoves = foldl doMove elves $ M.assocs unique
          doMove elves (to,from) = S.insert to $ S.delete (head from) elves

countEmpty :: S.Set (Int, Int) -> Int
countEmpty scan = ((yMax-yMin+1)*(xMax-xMin+1))-S.size scan
  where ((xMin,yMin),(xMax,yMax)) = bounds scan

main = do
    scan <- lines <$> readFile "day23.txt"
    let isSettled (_, _, settled) = settled
        (_, elves, _) = last $ take 11 $ iterate doRound (0, locations scan, False)
    -- putStrLn $ unlines $ display elves
    -- part 1
    print $ countEmpty elves
    -- part 2
    print $ pred $ length $ takeUntil isSettled $ iterate doRound (0, locations scan, False)

display :: S.Set (Int, Int) -> [String]
display scan = [ [ if (x,y) `S.member` scan then '#' else '.' | x <- [xMin..xMax]] | y <- [yMin..yMax]]
  where ((xMin,yMin),(xMax,yMax)) = bounds scan

bounds :: S.Set Point -> (Point, Point)
bounds points = ((xMin,yMin),(xMax,yMax))
  where (Min xMin, Min yMin, Max xMax, Max yMax) = foldMap (\(x,y) -> (Min x, Min y, Max x, Max y)) points
