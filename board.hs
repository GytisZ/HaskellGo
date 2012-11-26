import Data.Graph

type Board = Graph

-- value constructor to create n x n square board 
createSqBoard :: Int -> Board
createSqBoard n = buildG (1, n*n) undirected
    where 
        horizontal = [(x, x+1) | x <- [1..n*n], x `mod` n /= 0]
        vertical   = [(x, x+n) | x <- [1..n*n], x <= n*(n-1)]
        undirected = foldl (\acc (x,y) -> ((y,x):acc)
                   ) (horizontal ++ vertical) (horizontal ++ vertical)
