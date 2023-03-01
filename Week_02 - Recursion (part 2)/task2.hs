main :: IO()
main = do
    print $ sumDigitsRec 12345 == 15
    print $ sumDigitsRec 123 == 6

sumDigitsRec :: Int -> Int
sumDigitsRec 0 = 0
sumDigitsRec num = sumDigitsRec (div num 10) + (mod num 10)