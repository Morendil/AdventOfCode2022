{-# LANGUAGE NamedFieldPuns #-}
import Text.ParserCombinators.ReadP hiding (get)
import Data.Char (isNumber, isAlpha)
import Data.Maybe
import Data.Functor
import Data.List

type Monkey = (String, Expr)
data Expr = Expr String String String | Const Int
    deriving (Eq, Show)

eval :: [Monkey] -> String -> Int
eval monkeys name = case lookup name monkeys of
            Nothing -> error "No such monkey"
            Just (Const value) -> value
            Just (Expr op arg1 arg2) -> (operation op) (eval monkeys arg1) (eval monkeys arg2)

part1 :: [Monkey] -> Int
part1 monkeys = eval monkeys "root"

part2 :: [Monkey] -> Int
part2 monkeys = search monkeys

try :: Int -> [Monkey] -> (Int, Int)
try n monkeys = (eval monkeys' m1, eval monkeys' m2)
    where Expr _ m1 m2 = fromJust $ lookup "root" monkeys
          monkeys' = ("humn",Const n):(filter ((/=)"humn".fst) monkeys)

search :: [Monkey] -> Int
search monkeys = search' monkeys (signum $ high-low) startLow startHigh
    where low = fst $ try startLow monkeys
          high = fst $ try startHigh monkeys
          startLow = minBound `div` 1000
          startHigh = maxBound `div` 1000

search' :: [Monkey] -> Int -> Int -> Int -> Int
search' monkeys _ low high | low >= high = low
search' monkeys sign low high = if result == target then midpoint else if signum (target-result) == sign then searchHigh else searchLow
    where (result, target) = try midpoint monkeys
          midpoint = (low + high) `div` 2
          searchHigh = search' monkeys sign (midpoint+1) high
          searchLow = search' monkeys sign low midpoint

main = do
    monkeys <- fromJust . parseMaybe (sepBy1 monkey (string "\n")) <$> readFile "day21.txt"
    print $ part1 monkeys
    print $ part2 monkeys

operation :: String -> (Int -> Int -> Int)
operation " * " = (*)
operation " / " = div
operation " - " = (-)
operation " + " = (+)
operation _ = error "No such operation"

identifier = many1 (satisfy isAlpha)
constant = Const <$> number
operator = choice [string " * ", string " + ", string " - ", string " / "]
expression = do
    arg1 <- identifier
    op <- operator
    arg2 <- identifier
    return $ Expr op arg1 arg2
monkey = do
    name <- identifier
    string ": "
    expr <- constant +++ expression
    return (name, expr)

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

