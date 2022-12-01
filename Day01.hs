import Text.ParserCombinators.ReadP
import Data.Maybe (fromJust)
import Data.Char (isNumber)

number :: ReadP Int
number = read <$> many1 (satisfy isNumber)

elves = sepBy1 (sepBy1 number (char '\n')) (string "\n\n")

part1 :: [[Int]] -> Int
part1 = maximum . map sum

main = do
    inventory <- parseMaybe elves <$> readFile "day01.txt"
    print $ part1 $ fromJust inventory

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result