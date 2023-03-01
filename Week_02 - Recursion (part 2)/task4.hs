main :: IO()
main = do
    print $ isPrime 1 == False
    print $ isPrime 2 == True
    print $ isPrime 3 == True
    print $ isPrime 6 == False
    print $ isPrime 61 == True
    print $ roundTwoDigit 6734.3434534

-- roundTwoDigit n = (fromIntegral (round (n * 100))) / 100
-- roundTwoDigit n = (fromIntegral $ round $ n * 100) / 100
roundTwoDigit = (/ 100) . fromIntegral . round . (* 100) -- prefer this over the above!

isPrime :: Int -> Bool
isPrime n = n > 1 && helper 2
 where
    helper d
     | d >= n = True
     | mod n d == 0 = False
     | otherwise = helper (d + 1)
