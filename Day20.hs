import qualified Data.Map as M
import Data.Maybe
import Data.Tuple.Extra

type Encrypted = M.Map Int (Int,(Int, Int))

part1 :: [Int] -> Int
part1 numbers = flattened !! (1000`mod`n) + flattened !! (2000`mod`n) + flattened !! (3000`mod`n)
    where encrypted = encFromList numbers
          mixed = foldl (flip move) encrypted [1..n]
          flattened = encToList mixed
          n = length numbers

encFromList :: [Int] -> Encrypted
encFromList numbers = M.fromList $ zip [1..n] $ zip numbers $ zip (drop (n-1) (cycle [1..n])) (tail (cycle [1..n]))
    where n = length numbers

encToList :: Encrypted -> [Int]
encToList encrypted = take (length encrypted) $ map fst $ iterate right (fromJust $ M.lookup zero encrypted)
    where zero = head $ M.keys $ M.filter ((==0).fst) encrypted
          right (_,(_,r)) = fromJust $ M.lookup r encrypted

parse :: String -> Int
parse = read

move :: Int -> Encrypted -> Encrypted
move n encrypted = if value == 0 then encrypted else makeMove encrypted
    where start@(value,(leftOfStart, rightOfStart)) = fromJust $ M.lookup n encrypted
          right (_,(_,r)) = fromJust $ M.lookup r encrypted
          left (_,(l,_)) = fromJust $ M.lookup l encrypted
          step = if value < 0 then left else right
          swap = if value < 0 then reverse else id
          [(_,(_,newRight)), (_,(newLeft,_))] = swap $ drop (abs value) $ take (2+(abs value)) $ iterate step start
          makeMove = unlinkLeft . unlinkRight . linkLeft . linkRight . placeCell
          unlinkLeft = M.adjust (second (second $ const rightOfStart)) leftOfStart
          unlinkRight = M.adjust (second (first $ const leftOfStart)) rightOfStart
          linkRight = M.adjust (second (first $ const n)) newRight
          linkLeft = M.adjust (second (second $ const n)) newLeft
          placeCell = M.adjust (second $ const (newLeft, newRight)) n

main = do
    numbers <- map parse . lines <$> readFile "day20.txt"
    print $ part1 numbers
