import Text.ParserCombinators.ReadP
import Data.Maybe (fromJust)
import Data.Char (isNumber)
import Data.List
import Debug.Trace

data Tree = Leaf Int | Branch [Tree]
    deriving (Eq, Show)

number :: ReadP Int
number = read <$> many1 (satisfy isNumber)

packet = choice [Leaf <$> number, Branch <$> between (string "[") (string "]") ((sepBy packet (string ",")))]
pair = (,) <$> packet <*> (string "\n" *> packet)

instance Ord Tree where
    compare (Leaf a) (Leaf b) = compare a b
    compare (Branch []) (Branch []) = EQ
    compare (Branch []) (Branch _) = LT
    compare (Branch _) (Branch []) = GT
    compare (Branch (a:as)) (Branch (b:bs)) = case compare a b of
        LT -> LT
        GT -> GT
        EQ -> compare (Branch as) (Branch bs)
    compare (Leaf a) bb@(Branch b) = compare (Branch [Leaf a]) bb
    compare aa@(Branch a) (Leaf b) = compare aa (Branch [Leaf b])

part1 :: [(Tree,Tree)] -> Int
part1 = sum . map (+1) . findIndices (uncurry (<))

part2 :: [(Tree,Tree)] -> Int
part2 pairs = div1 * div2
    where div1 = 1 + fromJust (elemIndex add1 packets)
          div2 = 1 + fromJust (elemIndex add2 packets)
          add1 = Branch [Branch [Leaf 2]]
          add2 = Branch [Branch [Leaf 6]]
          packets = sort $ add1:add2:concatMap (\(a,b)->[a,b]) pairs

main = do
    pairs <- fromJust . parseMaybe (sepBy1 pair (string "\n\n")) <$> readFile "day13.txt"
    print $ part1 pairs
    print $ part2 pairs

parseMaybe :: ReadP a -> String -> Maybe a
parseMaybe parser input =
    case reverse $ readP_to_S parser input of
        [] -> Nothing
        ((result, _):_) -> Just result