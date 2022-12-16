{-# LANGUAGE NamedFieldPuns #-}
import Text.ParserCombinators.ReadP
import Algorithm.Search
import Data.Maybe (fromJust, mapMaybe)
import Data.Char (isNumber, isAlpha)
import Data.Functor (($>))
import Data.List
import qualified Data.Map as M

data Valve = Valve { name :: String, rate :: Int, exits :: [String]}
    deriving (Eq, Show)
data State = State { location :: String, open :: [String], totalRate :: Int, pressure :: Int, time :: Int }
    deriving (Eq, Show)

instance Ord State where
    compare State {location=l1,open=o1,time=t1,pressure=p1} State {location=l2,open=o2,time=t2,pressure=p2} = compare (p1,t1,o1,l1) (p2,t2,o2,l2)

type Chart = M.Map String Valve

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

part1 :: [Valve] -> [State]
part1 valves = fromJust path
    where charted = chart valves
          done state = sort (open state) == ["BB", "CC", "DD", "EE", "HH", "JJ"]
          start = State { location="AA", open=[], totalRate=0, pressure=0, time=30 }
          path = bfs (neighbours charted) done start

main = do
    valves <- fromJust . parseMaybe (sepBy1 valve (string "\n")) <$> readFile "day16_sample.txt"
    print $ part1 valves

valve :: ReadP Valve
valve = do
    string "Valve "
    name <- many1 (satisfy isAlpha)
    string " has flow rate="
    rate <- number
    choice [string "; tunnels lead to valves ", string "; tunnel leads to valve "]
    exits <- sepBy1 (many1 (satisfy isAlpha)) (string ", ")
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
