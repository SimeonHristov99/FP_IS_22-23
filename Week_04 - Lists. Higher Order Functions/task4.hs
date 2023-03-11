main :: IO()
main = do
    print $ isInside 1 5 4 == True -- x = 1, y = 5, n = 4
    print $ isInside 5 1 4 == True
    print $ isInside 10 50 20 == True
    print $ isInside 10 50 1 == False


