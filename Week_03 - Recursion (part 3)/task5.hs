main :: IO()
main = do
    print $ subNum 123 5123783 == True -- x = 123, y = 5123783
    print $ subNum 0 0 == True
    print $ subNum 10 101 == True
    print $ subNum 101 101 == True
    print $ subNum 10 0 == False
    print $ subNum 1253 5123783 == False
    print $ subNum 12 0 == False

-- sqAvg x y = fromIntegral $ x + y
-- sqAvg x y = fromIntegral (x + y)
-- sqAvg x y = div x y

numDigits :: Int -> Int
numDigits 0 = 0
numDigits n = 1 + numDigits (div n 10)

subNum :: Int -> Int -> Bool
subNum x y = helper y (numDigits x)
 where
    helper :: Int -> Int -> Bool
    helper num lenX
     | x > num = False
     | x == mod num (10^lenX) = True
     | otherwise = helper (div num 10) lenX