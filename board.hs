import Data.Graph

type Board = Graph
data Coordinate = Coordinate Int Int  -- coordinates for square board
type Game = [[Char]] -- Empty | Pos Game Coordinate 

-- value constructor to create n x n square board 
createSqBoard :: Int -> Board
createSqBoard n = buildG (1, n*n) undirected
    where 
        horizontal = [(x, x+1) | x <- [1..n*n], x `mod` n /= 0]
        vertical   = [(x, x+n) | x <- [1..n*n], x <= n*(n-1)]
        undirected = foldl (\acc (x,y) -> ((y,x):acc)
                   ) (horizontal ++ vertical) (horizontal ++ vertical)

-- check if a new stone is not on top of an old one
addStone :: Game  -> Coordinate -> Game
addStone game (Coordinate x y)  
    | (game !! x !! y == 'e') = newgame
    | otherwise               = game 
    where newgame = [ if i /= x then game !! i else newline |
                  i <- [0 .. (length game - 1)]]
          newline = [ if i /= y then game !! x !! i else 'b'|
                  i <- [0 .. (length game - 1)]] 
