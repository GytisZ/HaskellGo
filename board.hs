import Data.Graph

type Board = Graph

-- value constructor to create n x n square board 
createSqBoard :: Int -> Board
createSqBoard n = buildG (1, n*n) undirected
    where 
        horizontal = foldl (\acc x -> if x `mod` n == 0 
                                      then acc 
                                      else acc ++ [(x, x+1)]
                   ) [] [1..n*n]
        vertical   = foldl (\acc x -> if x > n*(n-1) 
                                      then acc 
                                      else acc ++ [(x,x+n)]
                   ) [] [1..n*n]
        undirected = foldl (\acc (x,y) -> acc ++ [(y,x)]
                   ) (horizontal ++ vertical) (horizontal ++ vertical)