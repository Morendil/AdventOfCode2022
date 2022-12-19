import Data.Char
import Data.Maybe
import Data.List
import Data.List.HT

type Stocks = [Int]
type Bots = [Int]
type State = (Stocks, Bots)
type Blueprint = [[Int]]

(ore,clay,obsidian,geode) = (0,1,2,3)

harvest :: State -> Stocks
harvest (stocks, bots) = zipWith (+) stocks bots

build :: Blueprint -> Stocks -> State -> Int -> Maybe State
build blueprint stocks' (stocks, bots) resource = if enough then Just (depleted,bots') else Nothing
    where costs = blueprint !! resource
          enough = all id $ zipWith (<=) costs stocks
          depleted = zipWith (-) stocks' (costs++repeat 0)
          bots' = (take resource bots)++[(bots !! resource)+1]++(drop (resource+1) bots)

step :: Blueprint -> State -> [State]
step blueprint s@(stocks, bots) = [(stocks',bots)]++mapMaybe (build blueprint stocks' s) (filter useful [0..3])
    where stocks' = harvest s
          maxCosts = take 3 $ foldl1 (zipWith max) $ map (padRight 0 4) $ blueprint
          useful 3 = True
          useful n = bots !! n < maxCosts !! n

expand :: Blueprint -> [State] -> [State]
expand blueprint states = concatMap (step blueprint) states

best :: Blueprint -> Int
best blueprint =  if toObsidian+toGeode > 24 then 0 else maximum $ map ((\s->s!!3).fst) $ last $ take (25-toObsidian-toGeode) $ iterate (expand blueprint) withGeode
    where buildObsidian = takeUntil (any (\s->(fst s)!!2>1)) $ iterate (expand blueprint) [([0,0,0,0],[1,0,0,0])]
          withObsidian = filter (\s->(fst s)!!2>0) $ last $ buildObsidian
          toObsidian = length buildObsidian -1
          buildGeode = takeUntil (any (\s->(fst s)!!3>0)) $ iterate (expand blueprint) withObsidian
          withGeode = filter (\s->(fst s)!!3>0) $ last $ buildGeode
          toGeode = length buildGeode - 1

part1 :: [Blueprint] -> [Int]
part1 = map best

parse :: String -> [[Int]]
parse = chunk . tail . map read . words . stripJunk
    where stripJunk = filter (\c -> isSpace c || isNumber c)
          chunk spec = [[spec !! 0], [spec !! 1], [spec !! 2, spec !! 3], [spec !! 4, 0, spec !! 5]]

main = do
    blueprints <- map parse . lines <$> readFile "day19.txt"
    print blueprints
    print $ part1 blueprints
