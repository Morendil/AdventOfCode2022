{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
import Text.ParserCombinators.ReadP
import Data.Maybe (fromJust, mapMaybe)
import Data.Char (isNumber, isAlpha)
import Data.Functor
import Data.List
import Data.List.HT (takeUntil)
import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S

data Valve = Valve { name :: String, rate :: Int, exits :: [(String,Int)]}
    deriving (Eq, Show)
data State = State { location :: String, open :: S.Set String, pressure :: Int, time::Int }
    deriving (Eq, Show)

instance Ord State where
    compare State {location=l1,open=o1,pressure=p1,time=t1} State {location=l2,open=o2,pressure=p2,time=t2} = compare (p1,t1,o1,l1) (p2,t2,o2,l2)

type Chart = M.Map String Valve
type Dists = M.Map (String, String) Int

chart :: [Valve] -> Chart
chart = foldMap insertOne
    where insertOne v@Valve {name} = M.insert name v M.empty

adjust :: [Valve] -> [Valve]
adjust valves = filter (not.isShut) $ go (filter isShut valves) valves
    where go [] valves = valves
          go (shut:_) valves = let rest = filter ((/=)(name shut).name) valves in go (filter isShut valves) (map (unlink shut) rest)

isShut Valve{name,rate} = name /= "AA" && rate==0

unlink :: Valve -> Valve -> Valve
unlink remove@Valve{name=xname,exits=rx} v@Valve{name,exits} = if xname `elem` (map fst exits) then v {exits=exits'} else v
    where exits' = M.assocs $ M.delete xname $ M.insert theOther merge $ M.fromList exits
          merge = (M.findWithDefault 1 theOther $ M.fromList rx) + (M.findWithDefault 1 xname $ M.fromList exits)
          theOther = head $ map fst rx \\ [name]

neighbours :: Chart -> State -> [State]
neighbours chart state@State { location, open, pressure, time } = if time == 0 then [state] else openHere ++ moves
    where get valve = fromJust $ M.lookup valve chart
          Valve {rate, exits} = get location
          moves = [state { location=name, time=max 0 time' } | (name,cost) <- exits, let time'=time-cost]
          openHere = if location `S.member` open || rate == 0 || time == 0 then [] else [state { open=S.insert location open, pressure=pressure+(rate*(time-1)), time=time-1 }]

advance :: Chart -> ([State], Bool) -> ([State], Bool)
advance chart (states, True) = (states, True)
advance chart (states, _) = (keepBest $ newStates, done)
    where newStates = concatMap (neighbours chart) states
          done = length newStates == length states
          keepBest = M.elems . M.fromListWith max . map (\s -> ((location s, open s),s))

part1 :: [Valve] -> Int
part1 valves = maximum $ map pressure $ fst $ last $ takeUntil snd $ iterate (advance charted) ([start],False)
    where charted = chart valves
          start = State { location="AA", open=S.empty, pressure=0, time=30 }

part2 :: [Valve] -> Int
part2 valves = maximum $ [pressure s1 + pressure s2 | s1:elephants <- tails pathsTaken, s2 <- elephants, S.disjoint (open s1) (open s2)]
    where charted = chart valves
          start = State { location="AA", open=S.empty, pressure=0, time=26 }
          pathsTaken = fst $ last $ takeUntil snd $ iterate (advance charted) ([start],False)

main = do
    valves <- fromJust . parseMaybe (sepBy1 valve (string "\n")) <$> readFile "day16_sample.txt"
    print $ part1 $ adjust valves
    print $ part2 $ adjust valves

display :: [Valve] -> [String]
display valves = map nodeSpec valves ++ concatMap edgeSpec valves
    where nodeSpec Valve { name , rate } = name ++ " [label = \"" ++ name ++ " (" ++ show rate ++ ")\"];"
          edgeSpec Valve { name , exits } = map (\s -> name ++ " -> " ++ s ++ ";") (map fst exits)

valve :: ReadP Valve
valve = do
    string "Valve "
    name <- many1 (satisfy isAlpha)
    string " has flow rate="
    rate <- number
    choice [string "; tunnels lead to valves ", string "; tunnel leads to valve "]
    exits <- map (,1) <$> sepBy1 (many1 (satisfy isAlpha)) (string ", ")
    return Valve {name, rate, exits}

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
