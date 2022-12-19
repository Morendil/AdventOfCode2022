import Data.Char
import Data.Maybe
import Data.List
import Data.List.HT
import qualified Data.Set as S

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
step blueprint s@(stocks, bots) = mapMaybe (build blueprint stocks' s) (filter useful [0..3])
    where stocks' = harvest s
          maxCosts = take 3 $ foldl1 (zipWith max) $ map (padRight 0 4) $ blueprint
          useful 3 = True
          useful n = bots !! n < maxCosts !! n

expand :: Blueprint -> ([State], S.Set Bots) -> ([State], S.Set Bots)
expand blueprint (states, inventories) = (waiting ++ buying, inventories')
    where buying = filter notRedundant $ S.toList $ S.fromList $ concatMap (step blueprint) states
          notRedundant = flip S.notMember inventories . snd
          inventories' = S.fromList $ map snd buying
          waiting = map (\s@(stocks,bots) -> (harvest s, bots)) states

best :: Int -> Blueprint -> Int
best n blueprint = if toObsidian+toGeode > n then 0 else maximum $ map ((\s->s!!3).fst) $ fst $ last $ take (n+1-toObsidian-toGeode) $ iterate (expand blueprint) withGeode'
    where buildObsidian = takeUntil (any (\s->(fst s)!!2>2).fst) $ iterate (expand blueprint) initial
          withObsidian = filter (\s->(fst s)!!2>0) $ fst $ last $ buildObsidian
          toObsidian = length buildObsidian -1
          buildGeode = takeUntil (any (\s->(fst s)!!3>0).fst) $ iterate (expand blueprint) withObsidian'
          withGeode = filter (\s->(fst s)!!3>0) $ fst $ last $ buildGeode
          toGeode = length buildGeode - 1
          -- 
          withInv s = (s, S.fromList $ map snd s)
          withObsidian' = withInv withObsidian
          withGeode' = withInv withGeode

initial = ([([0,0,0,0],[1,0,0,0])], S.empty)

part1 :: [Blueprint] -> Int
part1 = sum . zipWith (*) [1..] . map (best 24)

part2 :: [Blueprint] -> Int
part2 = product . map (best 32) . take 3

parse :: String -> [[Int]]
parse = chunk . tail . map read . words . stripJunk
    where stripJunk = filter (\c -> isSpace c || isNumber c)
          chunk spec = [[spec !! 0], [spec !! 1], [spec !! 2, spec !! 3], [spec !! 4, 0, spec !! 5]]

main = do
    blueprints <- map parse . lines <$> readFile "day19.txt"
    print $ part1 blueprints
    print $ part2 blueprints
