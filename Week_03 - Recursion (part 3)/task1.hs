main :: IO()
main = do
    print $ isNarcissistic 7 == True
    print $ isNarcissistic 12 == False
    print $ isNarcissistic 370 == True
    print $ isNarcissistic 371 == True
    print $ isNarcissistic 1634 == True

numDigits :: Int -> Int
numDigits 0 = 0
numDigits n = 1 + numDigits (div n 10)

isNarcissistic :: Int -> Bool
isNarcissistic n = n == helper (numDigits n) n
 where
    helper :: Int -> Int -> Int
    helper _ 0 = 0
    helper power leftover = (mod leftover 10)^power + helper power (div leftover 10)
