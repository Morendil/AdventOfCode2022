 {-# LANGUAGE DeriveFoldable #-}
import Data.Char
import Data.Maybe (fromJust)
import Test.Hspec

data GenericNode a = File String a | Dir String [GenericNode a]
    deriving (Eq, Show, Foldable)
type Node = GenericNode Int
type Path = [String]
type State = (Node, Path)

part1 :: Node -> Int
part1 = sum . map sum . collect forPart1

part2 :: Node -> Int
part2 tree = minimum $ filter (\x -> sum tree - x < 40000000) $ map sum $ collect isDir tree

forPart1 :: Node -> Bool
forPart1 (File _ _) = False
forPart1 dir = sum dir <= 100000

collect :: (Node -> Bool) -> Node -> [Node]
collect pred dir@(Dir _ nodes) = [dir | pred dir] ++ concatMap (collect pred) nodes
collect pred file = [file | pred file]

navigate :: [String] -> Node
navigate = fst . foldl execute (Dir "/" [], [])

execute :: State -> String -> State
-- Don't worry about these two
execute (node, path) cmd | cmd == "$ ls" = (node, path)
execute (node, path) cmd | cmd == "$ cd /" = (node, path)
-- Push or pop
execute (node, path) cmd | cmd == "$ cd .."  = (node, tail path)
execute (node, path) cmd | take 5 cmd == "$ cd "  = (node, drop 5 cmd:path)
-- Directory
execute (node, path) cmd | take 4 cmd == "dir " = (insert node (reverse path) (Dir (drop 4 cmd) []), path)
-- File
execute (node, path) cmd = (insert node (reverse path) (File name (read sizeS)), path)
    where (sizeS:name:rest) = words cmd

insert :: Node -> Path -> Node -> Node
insert (Dir name nodes) [] node = Dir name (nodes++[node])
insert (Dir name nodes) (cd:rest) node = Dir name (map (select cd rest node) nodes)
insert f@(File _ _) _ _ = error "Tried to cd into a file"

select :: String -> Path -> Node -> Node -> Node
select name path toInsert node@(Dir candidate nodes) | candidate == name = insert node path toInsert
select _ _ _ node = node

isDir :: Node -> Bool
isDir (Dir _ _ ) = True
isDir _ = False

main = do
    tree <- navigate . lines <$> readFile "day07.txt"
    print $ part1 tree
    print $ part2 tree

test :: IO ()
test = hspec $ do
  describe "Inserting" $ do
    it "inserts a directory at the top" $
        insert (Dir "/" []) [] (Dir "a" []) `shouldBe` Dir "/" [Dir "a" []]
    it "inserts below" $
        insert (Dir "/" [Dir "a" []]) ["a"] (File "foo" 15) `shouldBe` Dir "/" [Dir "a" [File "foo" 15]]
    it "inserts below again" $
        insert (Dir "/" [Dir "z" [], Dir "a" [Dir "b" []]]) ["a","b"] (File "foo" 15) `shouldBe` Dir "/" [Dir "z" [], Dir "a" [Dir "b" [File "foo" 15]]]
  describe "Executing" $ do
    it "executes cd" $
        execute (Dir "/" [], []) "$ cd a" `shouldBe` (Dir "/" [], ["a"])
    it "executes dir" $
        execute (Dir "/" [], []) "dir a" `shouldBe` (Dir "/" [Dir "a" []], [])
    it "executes file desc" $
        execute (Dir "/" [Dir "a" []], ["a"]) "15 foo" `shouldBe` (Dir "/" [Dir "a" [File "foo" 15]], ["a"])
    it "executes file desc nested" $
        foldl execute (Dir "/" [],[]) ["$ cd /","$ ls","dir a","$ cd a","$ ls","dir b","$ cd b","$ ls","54 i"] `shouldBe` (Dir "/" [Dir "a" [Dir "b" [File "i" 54]]], ["b","a"])
