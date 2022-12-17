{-# LANGUAGE NamedFieldPuns #-}
import Text.ParserCombinators.ReadP
import Algorithm.Search
import Data.Maybe (fromJust, mapMaybe)
import Data.Char (isNumber, isAlpha)
import Data.Functor (($>))
import Data.List
import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S

data Valve = Valve { name :: String, rate :: Int, exits :: [String]}
    deriving (Eq, Show)
data State = State { location :: String, open :: S.Set String, pressure :: Int }
    deriving (Eq, Show)

instance Ord State where
    compare State {location=l1,open=o1,pressure=p1} State {location=l2,open=o2,pressure=p2} = compare (p1,o1,l1) (p2,o2,l2)

type Chart = M.Map String Valve
type Dists = M.Map (String, String) Int

chart :: [Valve] -> Chart
chart = foldMap insertOne
    where insertOne v@Valve {name} = M.insert name v M.empty

neighbours :: Chart -> Int -> State -> [State]
neighbours chart time state@State { location, open, pressure } = openHere ++ moves
    where get valve = fromJust $ M.lookup valve chart
          Valve {rate, exits} = get location
          moves = map moveTo exits
          moveTo name = state { location=name }
          openHere = if location `S.member` open || rate == 0 then [] else [state { open=S.insert location open, pressure=pressure+(rate*(time-1)) }]

advance :: Chart -> (Int, [State]) -> (Int, [State])
advance chart (time, states) = (time-1, keepBest $ concatMap (neighbours chart time) states)
    where keepBest = M.elems . M.fromListWith max . map (\s -> ((location s, open s),s))

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

part1 :: [Valve] -> Int
part1 valves = length $ snd $ last $ take n $ iterate (advance charted) initial
    where charted = chart valves
          start = State { location="AA", open=S.empty, pressure=0 }
          initial = (n, [start])
          n = 30

part2 :: [Valve] -> Int
part2 valves = maximum $ [pressure s1 + pressure s2 | s1:elephants <- tails pathsTaken, s2 <- elephants, S.disjoint (open s1) (open s2)]
    where charted = chart valves
          start = State { location="AA", open=S.empty, pressure=0 }
          initial = (n, [start])
          n = 26
          pathsTaken = snd $ last $ take n $ iterate (advance charted) initial

main = do
    valves <- fromJust . parseMaybe (sepBy1 valve (string "\n")) <$> readFile "day16.txt"
    print $ part1 valves
    print $ part2 valves

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
