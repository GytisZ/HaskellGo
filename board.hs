import Data.Graph
import Data.Maybe
import Data.List

data Coordinate = Coordinate Int deriving(Eq, Show) 
data Colour     = Black | White deriving(Eq, Show)
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

-- elements from a triple
first :: (a, b, c) -> a  
first (x, _, _) = x  

second :: (a, b, c) -> b  
second (_, x, _) = x  

third :: (a, b, c) -> c  
third (_, _, x) = x  

-- get list of adjecent vertices
adjacent :: Graph -> Vertex -> [Vertex]
adjacent g v = [ u | u <- vertices g, elem (u, v) edgeset]
    where edgeset = edges g

showCoord :: Coordinate -> Int
showCoord (Coordinate x) = x

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
addStone board game (Coordinate x) col = output 
    where
        nghbFriendly   = [ i | i <- adjacent board x, fmap first (game i)
                       == Just col] 
        nghbUnfriendly = [ i | i <- adjacent board x, fmap first (game i) == 
                       Just (invertColour col) ]
        deadGroups     = concat [ second (fromJust (game i)) | i <- nghbUnfriendly
                       , numLiberties game (Coordinate i) == 1 ]
        newLiberties   = ((nub possLib ) \\ fmap Coordinate
                       ( x:nghbFriendly ++ nghbUnfriendly )) ++ deadLib
        possLib        = concat (catMaybes (map (fmap third) $ map game nghbFriendly) 
                       ) ++ (fmap Coordinate (adjacent board x))
        deadLib         = [Coordinate i | Coordinate i <- deadGroups, fmap Coordinate
                       ( adjacent board i ) `intersect` updatedGroup /= [] ]
        updatedGroup   = nub ( (Coordinate x): concat (catMaybes (map (fmap second) 
                       $ map game nghbFriendly)))
        output         = \v -> if 
                                   Coordinate v `elem` updatedGroup
                               then 
                                   Just (col, updatedGroup, newLiberties )
                               else if 
                                   Coordinate v `elem` deadGroups
                               then
                                   Nothing
                               else
                                   game v
