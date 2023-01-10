import Data.Char

main::IO()
main = do
    --print (getSuff 1234567 3)
    --print (reverseNum 123);
    --print (rev 123)
    --print (isDecreasing 1232)
    --print (numLen 123)


getSuff :: Int -> Int -> Int
getSuff num 1 = (rem num 10)
getSuff num count = ( getSuff (div num 10) (count-1) )* 10 + (rem num 10)

reverseNum :: Int -> Int
reverseNum num = helper num 0
    where 
        helper k ans = 
            if k < 10 then ans * 10 + k
            else helper (div k 10) (ans * 10 + (mod k 10))

getRevSuff :: Int -> Int -> Int
getRevSuff num c = (reverseNum (getRevSuff num c))

isDecreasing :: Int -> Bool
isDecreasing num = helper num (-1)
    where
        helper 0 _ = True
        helper num last =
            if(rem num 10) < last then False
            else helper (div num 10) (rem num 10)

numLen :: Int -> Int
numLen num = 
    if(num < 10) then 1
    else 1 + numLen (div num 10)

{-
reversOrdSuff :: Int -> Int
reversOrdSuff num = helper num 0 nemLen(num)
    where 
        helper num count len = 
            if(count == len) then if( isDecreasing( getRevSuff(num count) ))
            else 
                if( isDecreasing( getRevSuff(num count) ) ) then getRevSuff(num count)
-}

removeSames:: String -> String
removeSames str =
    if()

reduceStr:: String -> String
