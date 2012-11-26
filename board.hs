import Data.Graph
import Data.Time

type Board = Graph

-- value constructor to create n x n square board
createSqBoard :: Int -> Board
createSqBoard n = buildG (1, n*n) undirected
    where
        horizontal = foldl (\acc x -> if x `mod` n == 0
                                      then acc
                                      else acc ++ [(x, x+1)]
                   ) [] [1..n*n]
        vertical = foldl (\acc x -> if x > n*(n-1)
                                      then acc
                                      else acc ++ [(x,x+n)]
                   ) [] [1..n*n]
        undirected = foldl (\acc (x,y) -> acc ++ [(y,x)]
                   ) (horizontal ++ vertical) (horizontal ++ vertical)

createSqBoard' :: Int -> Board
createSqBoard' n = buildG (1, n*n) undirected
    where
        horizontal = foldl (\acc x -> if x `mod` n == 0
                                      then acc
                                      else ((x, x+1):acc)
                   ) [] [1..n*n]
        vertical = foldl (\acc x -> if x > n*(n-1)
                                    then acc
                                    else ((x, x+n):acc)
                   ) [] [1..n*n]
        undirected = foldl (\acc (x,y) -> ((y,x):acc)
                   ) (horizontal ++ vertical) (horizontal ++ vertical)

createSqBoard'' :: Int -> Board
createSqBoard'' n = buildG (1, n*n) undirected
    where
        horizontal = [(x, x+1) | x <- [1..n*n], x `mod` n /= 0]
        vertical = [(x, x+n) | x <- [1..n*n], x <= n*(n-1)]
        undirected = foldl (\acc (x,y) -> ((y,x):acc)
                   ) (horizontal ++ vertical) (horizontal ++ vertical)

timeCreate :: (Int -> Board) -> Int -> IO()
timeCreate creationFunc n = do
    let board = creationFunc n
    start <- getCurrentTime
    print $ board `seq` ()
    stop <- getCurrentTime
    print $ diffUTCTime stop start
