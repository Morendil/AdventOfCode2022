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

oneTurn :: Monkeys -> Int -> Monkeys
oneTurn monkeys index = emptyInventory $ foldl (inspectBy current) monkeys currentItems
    where current = fromJust $ IM.lookup index monkeys
          currentItems = items current
          emptyInventory afterTurn = IM.insert index (current {items=[], inspected=inspected current + length currentItems}) afterTurn

oneRound :: Monkeys -> Monkeys
oneRound monkeys = foldl oneTurn monkeys [0..length monkeys-1]

inspectBy :: Monkey -> Monkeys -> Int -> Monkeys
inspectBy inspector monkeys worryLevel = IM.insert target updated monkeys
    where Monkey { items=_, operation, args, test, rule } = inspector
          targeted = fromJust $ IM.lookup target monkeys
          updated = targeted {items=newWorry:items targeted}
          (arg1, arg2) = args
          newWorry = operation (arg1 worryLevel) (arg2 worryLevel) `div` 3
          target = if newWorry `mod` test == 0 then fst rule else snd rule

number :: ReadP Int
number = read <$> many1 (satisfy isNumber)

arg :: ReadP (Int -> Int)
arg = choice [old, const <$> number]
      where old = do; string "old"; return id

op :: ReadP (Int -> Int -> Int)
op = choice [mult, add]
     where mult = do; string " * "; return (*)
           add = do; string " + "; return (+)

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
    where final = last $ take 21 $ iterate oneRound monkeys

main = do
    monkeys <- IM.fromList . zip [0..] . fromJust . parseMaybe (sepBy1 monkey (string "\n\n")) <$> readFile "day11.txt"
    print $ part1 monkeys

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result