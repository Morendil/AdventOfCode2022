import Test.Hspec
import Data.Char (digitToInt, intToDigit)

snafuToDec :: String -> Int
snafuToDec = snafuToDec' . reverse
snafuToDec' [d] | d `elem` ['0','1','2'] = digitToInt d
snafuToDec' "-" = -1 
snafuToDec' "=" = -2
snafuToDec' [d] = error "Wrong digit"
snafuToDec' (d:rest) = snafuToDec' [d] + (5 * snafuToDec' rest)
snafuToDec' [] = error "No parse"

decToSnafu :: Int -> String
decToSnafu = reverse . decToSnafu'
decToSnafu' n | n < 3 = [snd $ snafuDigits !! n]
decToSnafu' n = snd lastDigit : decToSnafu' ((n-fst lastDigit)`div`5)
    where rem = n `mod` 5
          lastDigit = snafuDigits !! rem

snafuDigits = [(0,'0'),(1,'1'),(2,'2'),(-2,'='),(-1,'-')]

main = do
    fuelReqs <- lines <$> readFile "day25.txt"
    print $ decToSnafu $ sum $ map snafuToDec fuelReqs

test :: IO ()
test = hspec $ do
  describe "Converting SNAFU" $ do
    it "SNAFU to Decimal" $ do
        snafuToDec "1" `shouldBe` 1
        snafuToDec "2" `shouldBe` 2
        snafuToDec "12111" `shouldBe` 906
        snafuToDec "1=-0-2" `shouldBe` 1747
        snafuToDec "1121-1110-1=0"  `shouldBe` 314159265
    it "Decimal to SNAFU" $ do
        decToSnafu 906 `shouldBe` "12111"
        decToSnafu 1747 `shouldBe` "1=-0-2"
        decToSnafu 314159265 `shouldBe` "1121-1110-1=0"  
