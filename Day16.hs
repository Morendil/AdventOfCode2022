{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
import Text.ParserCombinators.ReadP hiding (get, count)
import Data.Maybe (fromJust, mapMaybe, isNothing)
import Data.Char (isNumber, isAlpha)
import Data.Functor
import Data.List
import Data.List.HT (takeUntil)
import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S
import Algorithm.Search
import Control.Monad.State

data Valve = Valve { name :: String, rate :: Int, exits :: [(String,Int)]}
    deriving (Eq, Show)
data World = World { location :: String, open :: S.Set String, pressure :: Int, time::Int }
    deriving (Eq, Show)
data Global = Global { count::Int, scores::M.Map (String, S.Set String) Int }
    deriving (Eq, Show)

instance Ord World where
    compare World {location=l1,open=o1,pressure=p1,time=t1} World {location=l2,open=o2,pressure=p2,time=t2} = compare (p1,t1,o1,l1) (p2,t2,o2,l2)

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
unlink remove@Valve{name=xname,exits=rx} v@Valve{name,exits} = if xname `elem` map fst exits then v {exits=exits'} else v
    where exits' = M.assocs $ M.delete xname $ M.insert theOther merge $ M.fromList exits
          merge = M.findWithDefault 1 theOther (M.fromList rx) + M.findWithDefault 1 xname (M.fromList exits)
          theOther = head $ map fst rx \\ [name]

neighbours :: Chart -> World -> [World]
neighbours chart state@World { location, open, pressure, time } = if time == 0 then [state] else openHere ++ moves
    where get valve = fromJust $ M.lookup valve chart
          Valve {rate, exits} = get location
          moves = [state { location=name, time=max 0 time' } | (name,cost) <- exits, let time'=time-cost]
          openHere = [state { open=S.insert location open, pressure=pressure+(rate*(time-1)), time=time-1 } | not (location `S.member` open || rate == 0 || time == 0)]

neighboursM :: Chart -> World -> State Global [World]
neighboursM chart world = do
    Global {count, scores} <- get
    let newStates = filter isImprovement $ neighbours chart world
        isImprovement World {location,open,pressure} = let found = M.lookup (location, open) scores in isNothing found || pressure > fromJust found
        newScores = map (\s -> ((location s, open s),pressure s)) newStates
        scores' = M.union (M.fromList newScores) scores
    modify $ \ global -> global { count=count + length newStates, scores=scores' }
    return newStates

part1 :: [Valve] -> Int
part1 valves = maximum $ M.elems $ scores $ snd result
    where charted = chart $ adjust valves
          goalM w = return False
          start = World { location="AA", open=S.empty, pressure=0, time=30 }
          process = bfsM (neighboursM charted) goalM start
          result = runState process initial
          initial = Global { count=0, scores=M.empty }

main = do
    valves <- fromJust . parseMaybe (sepBy1 valve (string "\n")) <$> readFile "day16.txt"
    print $ part1 $ adjust valves

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
