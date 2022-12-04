import Text.ParserCombinators.ReadP
import Data.Char (isNumber)
import Data.Maybe (fromJust)

type Section = (Int, Int)
type Assignment = (Section, Section)

number = read <$> many1 (satisfy isNumber)
range =  (,) <$> number <*> (string "-" *> number)
assignment =  (,) <$> range <*> (string "," *> range)
assignments = sepBy1 assignment (string "\n")

containment :: Assignment -> Bool
containment (s1@(min1,max1),s2@(min2,max2)) = belong s1 (amin,amax) || belong s2 (amin,amax)
    where amin = minimum [min1,max1,min2,max2]
          amax = maximum [min1,max1,min2,max2]
          belong r (smin,smax) = inRange smin r && inRange smax r

overlap :: Assignment -> Bool
overlap (s1@(min1,max1),s2@(min2,max2)) = inRange max1 s2 || inRange min1 s2 || inRange max2 s1 || inRange min2 s1

inRange :: Int -> Section -> Bool
inRange x (rmin,rmax) = x >= rmin && x <= rmax

part1 :: [Assignment] -> Int
part1 = length . filter containment

part2 :: [Assignment] -> Int
part2 = length . filter overlap

main = do
    cleanup <- fromJust . parseMaybe assignments <$> readFile "day04.txt"
    print $ part1 cleanup
    print $ part2 cleanup

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result