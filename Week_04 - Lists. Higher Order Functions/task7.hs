main :: IO()
main = do
    print $ incrementByLC 5 [5] == [10]
    print $ incrementByLC 4 [4, 4] == [8, 8]
    print $ incrementByLC 5 [1] == [6]
    print $ incrementByLC 5 [5, 1, 5, 3, 5] == [10, 6, 10, 8, 10]
    print $ incrementByLC 3 [5, 1, 5, 3, 5] == [8, 4, 8, 6, 8]
        
    print $ incrementByHOF 5 [5] == [10]
    print $ incrementByHOF 4 [4, 4] == [8, 8]
    print $ incrementByHOF 5 [1] == [6]
    print $ incrementByHOF 5 [5, 1, 5, 3, 5] == [10, 6, 10, 8, 10]
    print $ incrementByHOF 3 [5, 1, 5, 3, 5] == [8, 4, 8, 6, 8]


