main :: IO()
main = do
    --print 123
    --print "abc"
    --print a
    --print (f1 10 a)
    --print (myMin 5 8)
    print (isInside 7 8 10)


a :: Int
a = 12

f1 :: Int -> Int -> Int
f1 a b = a * (2 + b)

myMin :: Int ->Int -> Int
myMin a b = 
    if a < b then a else b

isInside :: Int ->  Int -> Int -> Bool
isInside x a b =
    --if x > a && x < b then True else False
    x > a && x < b

