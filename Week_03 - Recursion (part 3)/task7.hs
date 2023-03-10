main :: IO()
main = do
    print $ mySin 100 1 == 0.8414709848078965 -- n = 100, x = 1
    print $ mySin 100 0.5 == 0.479425538604203

factIter :: Integer -> Integer
factIter n = helper 1 n
 where
    helper result 0 = result
    helper result leftover = helper (result * leftover) (leftover - 1)

mySin :: Integer -> Double -> Double
mySin 0 x = x
mySin n x = ((-1)^n * x^(2 * n + 1)) / (fromIntegral $ factIter $ 2 * n + 1) + mySin (n - 1) x
