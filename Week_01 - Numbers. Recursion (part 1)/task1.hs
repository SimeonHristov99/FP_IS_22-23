main :: IO()
main = do
    print $ min 5 6 == 5
    print $ min 5 (-6) == (-6)
    print $ minIf 15 60 == 15
    print $ minIf 60 15 == 15
    print $ minGuard 15 60 == 15
    print $ minGuard 60 15 == 15
    print $ minBuiltIn 60 15 == 15

    print $ lastDigit 154 == 4

    print $ quotientWhole 64 2 == 32
    print $ divWhole 154 17 == 9.058823529411764

    print $ removeLastDigit 154 == 15    

    print $ divReal 154.451 10.01 == 15.42967032967033
    print $ quotientReal 154.21 17.17 == 8

    print $ avgWhole 5 1542 == 773.5

    print $ roundTwoDig 3.1413465345321 == 3.14
    print $ roundTwoDigButWithMagic 3.1413465345321 == 3.14 -- partial function application and composition (defining a function at functional level)

roundTwoDigButWithMagic :: Double -> Double
roundTwoDigButWithMagic = (/ 100) . fromIntegral . round . (* 100)
-- roundTwoDigButWithMagic x = ((/ 100) . fromIntegral . round . (x * 100) x)

roundTwoDig :: Double -> Double
roundTwoDig n = (fromIntegral $ round $ n * 100) / 100

avgWhole :: Int -> Int -> Double
avgWhole x y = (fromIntegral $ x + y) / 2

quotientReal :: Double -> Double -> Int
quotientReal x y = truncate $ x / y

divReal :: Double -> Double -> Double
divReal x y = x / y

removeLastDigit :: Int -> Int
removeLastDigit n = div n 10

divWhole :: Int -> Int -> Double
divWhole x y = (fromIntegral x) / (fromIntegral y)

quotientWhole :: Int -> Int -> Int
quotientWhole x d = div x d
-- quotientWhole x d = x `div` d

lastDigit :: Int -> Int
lastDigit n = mod n 10

minBuiltIn :: Int -> Int -> Int
minBuiltIn x y = min x y

minGuard :: Int -> Int -> Int
minGuard x y
 | x < y = x
 | otherwise = y

minIf :: Int -> Int -> Int
minIf x y = if x < y then x else y
