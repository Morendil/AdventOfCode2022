{-# LANGUAGE NamedFieldPuns #-}
import Text.ParserCombinators.ReadP
import Data.Maybe (fromJust)
import Data.Char (isNumber)
import Data.List
import Data.Ord
import qualified Data.IntMap as IM

data Monkey = Monkey {
    items :: [Int],
    operation :: (Int -> Int -> Int),
    args :: ((Int -> Int), (Int -> Int)),
    test :: Int,
    rule :: (Int, Int),
    inspected :: Int
}

instance Show Monkey where
    show Monkey { items, operation, args, test, rule, inspected } = "holds " ++ show items ++ " (inspected " ++ show inspected ++ ")"

type Monkeys = IM.IntMap Monkey

oneRound :: Int -> Monkeys -> Monkeys
oneRound zenLevel monkeys = foldl (oneTurn zenLevel) monkeys [0..length monkeys-1]

oneTurn :: Int -> Monkeys -> Int -> Monkeys
oneTurn zenLevel monkeys index = emptyInventory $ foldl (inspectBy zenLevel current) monkeys currentItems
    where current = fromJust $ IM.lookup index monkeys
          currentItems = items current
          emptyInventory afterTurn = IM.insert index (current {items=[], inspected=inspected current + length currentItems}) afterTurn

inspectBy :: Int -> Monkey -> Monkeys -> Int -> Monkeys
inspectBy zenLevel inspector monkeys worryLevel = IM.insert target updated monkeys
    where Monkey { items=_, operation, args, test, rule } = inspector
          targeted = fromJust $ IM.lookup target monkeys
          updated = targeted {items=newWorry:items targeted}
          (arg1, arg2) = args
          increasedWorry = operation (arg1 worryLevel) (arg2 worryLevel)
          -- This is a bit quick and dirty… part1/part2 logic switch
          newWorry = if zenLevel == 0 then increasedWorry `div` 3 else increasedWorry `mod` zenLevel
          target = if newWorry `mod` test == 0 then fst rule else snd rule

number :: ReadP Int
number = read <$> many1 (satisfy isNumber)

arg :: ReadP (Int -> Int)
arg = choice [string "old" *> return id, const <$> number]

op :: ReadP (Int -> Int -> Int)
op = choice [string " * " *> return (*), string " + " *> return (+)]

monkey :: ReadP Monkey
monkey = do
    string "Monkey " *> number <* string ":\n"
    items <- string "  Starting items: " *> sepBy1 number (string ", ") <* string "\n"
    arg1 <- string "  Operation: new = " *> arg
    operation <- op
    arg2 <- arg <* string "\n"
    test <- string "  Test: divisible by " *> number <* string "\n"
    ifTrue <- string "    If true: throw to monkey " *> number <* string "\n"
    ifFalse <- string "    If false: throw to monkey " *> number
    return Monkey { items, operation, args=(arg1,arg2), test, rule=(ifTrue, ifFalse), inspected=0 }

part1 :: Monkeys -> Int
part1 monkeys = product $ take 2 $ reverse $ sort $ map inspected $ IM.elems final
    where final = last $ take 21 $ iterate (oneRound 0) monkeys

part2 :: Monkeys -> Int
part2 monkeys = product $ take 2 $ reverse $ sort $ map inspected $ IM.elems final
    where final = last $ take 10001 $ iterate (oneRound zenLevel) monkeys
          zenLevel = product $ map test $ IM.elems monkeys

main = do
    monkeys <- IM.fromList . zip [0..] . fromJust . parseMaybe (sepBy1 monkey (string "\n\n")) <$> readFile "day11.txt"
    print $ part1 monkeys
    print $ part2 monkeys

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result