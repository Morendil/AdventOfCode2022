{-# LANGUAGE NamedFieldPuns #-}
import Text.ParserCombinators.ReadP
import Algorithm.Search
import Data.Maybe (fromJust, mapMaybe)
import Data.Char (isNumber, isAlpha)
import Data.Functor (($>))
import Data.List
import Data.Ord
import qualified Data.Map as M

data Valve = Valve { name :: String, rate :: Int, exits :: [String]}
    deriving (Eq, Show)
data State = State { location :: String, open :: [String], totalRate :: Int, pressure :: Int, time :: Int }
    deriving (Eq, Show)

instance Ord State where
    compare State {location=l1,open=o1,time=t1,pressure=p1} State {location=l2,open=o2,time=t2,pressure=p2} = compare (p1,t1,o1,l1) (p2,t2,o2,l2)

type Chart = M.Map String Valve
type Dists = M.Map (String, String) Int

chart :: [Valve] -> Chart
chart = foldMap insertOne
    where insertOne v@Valve {name} = M.insert name v M.empty

-- neighbours :: Chart -> State -> [State]
-- neighbours chart State { location, open, totalRate, pressure, time } = stay:moves
--     where get valve = fromJust $ M.lookup valve chart
--           Valve {exits} = get location
--           moves = concatMap moveTo exits
--           stay = justMove location
--           moveTo name = let Valve {rate} = get name in if name `elem` open || rate == 0 then [justMove name] else [moveAndOpen name rate, justMove name]
--           moveAndOpen name rate =  State { location=name, open=name:open, totalRate=totalRate+rate, pressure=pressure-(totalRate*2), time=time-2 }
--           justMove name = State { location=name, open=open, totalRate=totalRate, pressure=pressure-totalRate, time=time-1 }

neighbours :: Chart -> State -> [State]
neighbours chart State { location, open, totalRate, pressure, time } = moves
    where get valve = fromJust $ M.lookup valve chart
          Valve {exits} = get location
          moves = concatMap moveTo exits
          moveTo name = let Valve {rate} = get name in if name `elem` open || rate == 0 then [justMove name] else [moveAndOpen name rate, justMove name]
          moveAndOpen name rate =  State { location=name, open=name:open, totalRate=totalRate+rate, pressure=pressure-(totalRate*2), time=time-2 }
          justMove name = State { location=name, open=open, totalRate=totalRate, pressure=pressure-totalRate, time=time-1 }

dist :: Chart -> String -> String -> Int
dist chart from to = length $ fromJust $ bfs neighbours (==to) from
    where get valve = fromJust $ M.lookup valve chart
          neighbours v = exits $ get v

value :: Chart -> Int -> String -> String -> Int
value chart time from to = (rate $ get to) * (time - (dist chart from to + 1))
    where get valve = fromJust $ M.lookup valve chart

score :: Chart -> (Int, Int, String) -> String -> (Int, Int, String)
score chart (before,time,from) to = (before+(time'*(rate $ get to)), time', to)
    where get valve = fromJust $ M.lookup valve chart
          cost = 1 + dist chart from to
          time' = time - cost

score' :: Dists -> Chart -> (Int, Int, String) -> String -> (Int, Int, String)
score' dists chart (before,time,from) to = (before+(time'*(rate $ get to)), time', to)
    where get valve = fromJust $ M.lookup valve chart
          cost = 1 + (fromJust $ M.lookup (from,to) dists)
          time' = time - cost

dists :: [Valve] -> Dists
dists valves = M.fromList $ [((from,to),dist charted from to) | from<-map name valves, to<-map name valves]
    where charted = chart valves

part1 :: [Valve] -> [State]
part1 valves = fromJust path
    where charted = chart valves
          done state = sort (open state) == ["BB", "CC", "DD", "EE", "HH", "JJ"]
          start = State { location="AA", open=[], totalRate=0, pressure=0, time=30 }
          path = bfs (neighbours charted) done start

main = do
    valves <- fromJust . parseMaybe (sepBy1 valve (string "\n")) <$> readFile "day16_sample.txt"
    let charted = chart valves
        unstuck = map name $ filter ((/=)0 . rate) valves
    print $ maximumBy (comparing (foldl (score charted) (0,30,"AA"))) $ permutations unstuck

valve :: ReadP Valve
valve = do
    string "Valve "
    name <- many1 (satisfy isAlpha)
    string " has flow rate="
    rate <- number
    choice [string "; tunnels lead to valves ", string "; tunnel leads to valve "]
    exits <- sepBy1 (many1 (satisfy isAlpha)) (string ", ")
    return Valve {name, rate, exits}

display :: [Valve] -> [String]
display valves = map nodeSpec valves ++ concatMap edgeSpec valves
    where nodeSpec Valve { name , rate } = name ++ " [label = \"" ++ name ++ " (" ++ show rate ++ ")\"];"
          edgeSpec Valve { name , exits } = map (\s -> name ++ " -> " ++ s ++ ";") exits

number :: ReadP Int
number = do
    sign <- option 1 (string "-" $> (-1))
    mag <- read <$> many1 (satisfy isNumber)
    return $ sign * mag

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result
