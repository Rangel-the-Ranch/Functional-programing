import Data.List
import Data.Char


main :: IO()
main = do
    --print (generate [10, 15, 25] [1, 5, 20, 30])
    inputFile <- readFile "input.txt"
    print (countUniques inputFile)


generate ::[Int] -> [Int] ->[[Int]]
generate as bs = [[1]]




------------------------------------------------------------------------------------------------------------
-- 2 ,4 ,3 ,7

getLinesOfFile :: String -> Int
getLinesOfFile str 
    = (length (lines str))

stringToList :: String -> [String]
stringToList str = words str

isLookedNumber :: String -> Bool
isLookedNumber str = (length str == 2) || (length str == 4) || (length str == 3) ||(length str == 7)   

-- Намираме броя на думите с дължина 2,3,4,7 изваждаме за всеки ред 4 и тове е отговора
countUniques :: String -> Int
countUniques str = ( helper (stringToList str) - (getLinesOfFile str) * 4 )
    where 
        helper [] = 0
        helper xs
            = if isLookedNumber (head xs) 
                then 1 + helper (tail xs)
                else helper (tail xs)