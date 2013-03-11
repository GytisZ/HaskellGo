import Data.Graph
import Data.Maybe

data Coordinate = Coordinate Int deriving(Eq) 
data Colour     = Black | White deriving(Eq)
type Board      = Graph 
-- every stone contains has the information about all stones in
-- it's group and and the liberties
type Game       = Vertex -> Maybe ( Colour, [ Coordinate ], [ Coordinate ] ) 

-- value constructor to create n x n square board 
createSqBoard :: Int -> Board
createSqBoard n = buildG (1, n*n) undirected
    where 
        horizontal = [(x, x+1) | x <- [1..n*n], x `mod` n /= 0]
        vertical   = [(x, x+n) | x <- [1..n*n], x <= n*(n-1)]
        undirected = foldl (\acc (x,y) -> ((y,x):acc)
                   ) (horizontal ++ vertical) (horizontal ++ vertical)

-- invert colours
invertColour :: Colour -> Colour
invertColour Black = White
invertColour White = Black

-- first element from a triple
first :: (a, b, c) -> a  
first (x, _, _) = x  

-- get list of adjecent vertices
adjacent :: Graph -> Vertex -> [Vertex]
adjacent g v = [ u | u <- vertices g, elem (u, v) edgeset]
    where edgeset = edges g

-- check if the given coordinate is on the board
isValidCoordinate :: Board -> Coordinate -> Bool
isValidCoordinate board (Coordinate x) = elem x (vertices board)

-- check if there is a stone on a given position
isEmpty :: Game -> Coordinate -> Bool
isEmpty game (Coordinate x) = isNothing(game x)

-- get a number of liberties for the group of a given stone
numLiberties :: Game -> Coordinate -> Int
numLiberties game (Coordinate x) = length $ liberties
    where
        Just ( _, _, liberties ) = game x

-- check if the move is a suicide 
isSuicide :: Board -> Game -> Coordinate -> Colour -> Bool
isSuicide board game (Coordinate x) col =  isSurrounded && doesntKill  
    where 
        isSurrounded = and [ if 
                                 game i == Nothing
                             then
                                 False
                             else if
                                 fmap first (game i) == Just (invertColour col)
                             then
                                 True
                             else
                                 numLiberties game (Coordinate i) == 1 
                     | i <- (adjacent board x)]
        doesntKill   =  and [ numLiberties game (Coordinate i) > 1 
                     | i <- (adjacent board x), fmap first (game i) 
                     == Just (invertColour col)]

-- add a new stone to the game 
addStone :: Board -> Game  -> Coordinate -> Colour -> Game
addStone = undefined  

