main :: IO()
main = do
    print $ hasIncDigits 1244 == True
    print $ hasIncDigits 12443 == False
    
    print $ hasIncDigitsV1 1244 == True
    print $ hasIncDigitsV1 12443 == False
    
    print $ hasIncDigitsV2 1244 == True
    print $ hasIncDigitsV2 12443 == False

hasIncDigits :: Int -> Bool
hasIncDigits 0 = True
hasIncDigits n = (hasIncDigits $ div n 10) && mod n 10 >= mod (div n 10) 10

-- alternative solution 1
hasIncDigitsV1 :: Int -> Bool
hasIncDigitsV1 n = n < 10 || mod n 10 >= mod (div n 10) 10 && hasIncDigitsV1 (div n 10)

-- alternative solution 2
hasIncDigitsV2 :: Int -> Bool
hasIncDigitsV2 0 = True
hasIncDigitsV2 n
 | mod n 10 >= mod (div n 10) 10 = hasIncDigitsV2 $ div n 10
 | otherwise = False
