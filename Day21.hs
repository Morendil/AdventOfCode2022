{-# LANGUAGE NamedFieldPuns #-}
import Text.ParserCombinators.ReadP hiding (get)
import Data.Char (isNumber, isAlpha)
import Data.Maybe
import Data.Functor
import qualified Data.Map as M
import Control.Monad.Trans.State

type Monkey = (String, Expr)
data Expr = Expr String String String | Const Int
    deriving (Eq, Show)
type Cache = M.Map String Int

eval :: [Monkey] -> String -> State Cache Int
eval monkeys name = do
    cache <- get
    let cached = M.lookup name cache
    if isJust cached then return $ fromJust cached
    else case lookup name monkeys of
            Nothing -> error "No such monkey"
            Just (Const value) -> return value
            Just (Expr op arg1 arg2) -> do
                val1 <- eval monkeys arg1
                val2 <- eval monkeys arg2
                let result = (operation op) val1 val2
                return result

part1 :: [Monkey] -> Int
part1 monkeys = evalState (eval monkeys "root") M.empty

main = do
    monkeys <- fromJust . parseMaybe (sepBy1 monkey (string "\n")) <$> readFile "day21.txt"
    print $ part1 monkeys

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

