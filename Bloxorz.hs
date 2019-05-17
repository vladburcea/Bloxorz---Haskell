{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState

import qualified Data.Array as A

-- Representation of the game:
hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'


-- Data type that represents a set of coordinates as a pair of Ints
type Position = (Int, Int) 

-- Function that adds the values of 2 positions
addPositions :: Position -> Position -> Position
addPositions (a,b) (c,d) = (a + c, b + d)

-- For every uninitialised position
naPosition :: Position
naPosition = (-2,-2)

-- Directions in which the block can move
data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

directionList :: [Directions]
directionList = [East, South, West, North]

-- Data type representing each type of cell
data Cell = HardTile | SoftTile | Block | Switch | EmptySpace | WinningTile
    deriving (Eq, Ord)

instance Show Cell where
    show c = case c of 
        HardTile -> [hardTile]
        SoftTile -> [softTile]
        Block -> [block]
        Switch -> [switch]
        EmptySpace -> [emptySpace]
        WinningTile -> [winningTile]
        
-- List of all the cells that trigger an efect after stepping on one of them
activCells :: [Cell] 
activCells = [SoftTile, Switch, EmptySpace, WinningTile]

{-
    Data type with which a level is represented:

    matrix -> the map
    sw -> a list of pairs containing the coords of a switch and the list of tiles it affects
    blck -> a tuple that contains the positions of the 2 cubes forming the block
    gs -> game state: 1 means game won, -1 game over and 0 still playing
-}

data Level = Level {
                matrix :: (A.Array Position Cell),
                sw :: [(Position, [Position])],
                blck :: (Position, Position),
                gs :: Int
                }
    deriving (Eq, Ord)

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

instance Show Level where
    show level = "\n" ++ unlines [concat [show (mat A.! (y, x)) | x <- [0..n]] | y <- [0..m]] ++ finishMessage
        where 
            mat = case (snd $ blck level) of
                (-2, -2) -> ((matrix level) A.// [((fst (blck level)), Block)])
                (_, _) -> ((matrix level) A.// [((fst (blck level)), Block), ((snd (blck level)), Block)])
            finishMessage = if ((gs level) == -1)
                then "Game Over\n" 
                else 
                    if (gs level) == 1
                        then "Congrats! You won!\n" 
                        else ""
            n = snd $ snd $ A.bounds $ matrix level
            m = fst $ snd $ A.bounds $ matrix level

{-
    @p1 -> lower right corner of the map (to determin the size of the map)
    @p2 -> coords of the block 

    This function returns an empty level with the block placed at coords p2, in
    vertical orientation.

    When the block is in vertical orientation the 2nd's set of coords for the 
    block is (-2, -2).
-}

emptyLevel :: Position -> Position -> Level
emptyLevel p1 p2 = (Level 
        (A.listArray ((0,0),  p1) (repeat EmptySpace))
        []
        (p2, naPosition)
        0)
            
{-
    @tType -> tile type: 
        'H' -> tile hard 
        'S' -> tile soft 
        'W' -> winning tile 
    @p -> position of where to place the tile
    @lvl -> the lvl that's changed

    Function that adds a certain type of tile at a position
-}

addTile :: Char -> Position -> Level -> Level
addTile tType p lvl = lvl {matrix = ((matrix lvl) A.// [(p, tileType)])}
    where tileType = case tType of
             'H' -> HardTile
             'S' -> SoftTile
             _ -> WinningTile

{-
    @p -> position of the switch
    @pL -> list of positions that the switch affects
    @lvl -> the lvl that's changed
    
    Function that adds a switch on the map. Similar to "addTile", but also 
    stores the information about the tiles that the switch affects
-}

addSwitch :: Position -> [Position] -> Level -> Level
addSwitch p pL lvl = lvl { matrix = ((matrix lvl) A.// [(p, Switch)]), sw = ((p,pL) : (sw lvl) ) }
    
{-
    === BLOCK MOVEMENT ===
-}

{-
    Function that returns the oposit type of Cell for a switch affected tile

    It returns a HardTile if given an EmptySpace and vice versa
-} 

oposing :: Cell -> Cell
oposing c = case c of 
    HardTile -> EmptySpace
    _ -> HardTile

{-
    @cPos -> the position of the switch to activate
    @lvl -> level for which the switch is activated

    This function activates a switch at a given position. It changes all the 
    tiles affected by the switch.
-}

activateSw :: Position -> Level -> Level
activateSw cPos lvl = if (((matrix lvl) A.! cPos) == Switch) then
    lvl {matrix = ((matrix lvl) A.// zip switchTiles (repeat newType))}
    else lvl
    where
        switchTiles = snd $ head $ filter ((== cPos) . fst) (sw lvl)
        newType = oposing $ matrix lvl A.! (head $ snd $ head $ filter ((== cPos) .fst) (sw lvl))

{-
    Function that checks if the block is still on the map or if it fell.
    
    If the block fell then then game state is updated to -1 (loss).
-}

fellOff :: Level -> Bool
fellOff lvl = if (matrix lvl A.! cube1) == EmptySpace || ((cube2 /= naPosition) && (matrix lvl A.! cube2 == EmptySpace)) then True else False
    where 
        cube1 = fst $ blck lvl
        cube2 = snd $ blck lvl

{-
    @c -> type of cell to activate
    @lvl -> level to change

    The function returns a new level, with the changes made by the activation
    of the cell, if the block is positioned on a block of that type and if it
    isn't it returns the lvl unchanged
-}

activate :: Cell -> Level -> Level
activate c lvl = case c of 
    Switch ->  
        if (snd $ blck lvl) == naPosition 
            then 
                activateSw (fst $ blck lvl) lvl 
            else 
                activateSw (snd $ blck lvl) $ activateSw (fst $ blck lvl) lvl 
    SoftTile -> 
        if (snd $ blck lvl) == naPosition && (((matrix lvl A.! (fst $ blck lvl))) == SoftTile)
            then lvl {gs = -1} 
             else lvl
    EmptySpace -> 
        if fellOff lvl then lvl {gs = -1} else lvl
    WinningTile -> 
        if (((snd $ blck lvl) == naPosition) && ((matrix lvl A.! (fst $ blck lvl))) == WinningTile) then lvl {gs = 1} else lvl
    _ -> lvl

{-
    Directional offset for every possible move, calculated heuristicly.

    The order of the cardinal points associated with the offset is 
    N E S W (clockwise)
-}

directionalOffset :: [Position]
directionalOffset = [(-1, 0), (0, 1), (1, 0), (0,-1)]

-- Direction parsing
getDirection :: Directions -> Int
getDirection c = case c of
    North -> 0
    East -> 1
    South -> 2
    _ -> 3

{-
    getMostDP -> get most distant point. When the block is in horizontal 
                 position and the move that is requested makes the block change
                 orientation, the most distant point is necessary.
    Ex:
        move East lvl:

        0 1 2 3 4 5                0 1 2 3 4 5
    0   _ _ _ _ _ _             0  _ _ _ _ _ _
    1   _ # # _ _ _             1  _ _ _ # _ _ 
    2   _ _ _ _ _ _      ->     2  _ _ _ _ _ _ 
    3   _ _ _ _ _ _             3  _ _ _ _ _ _
        Initial lvl                After move

        mostDP in this case is the second cube of the block (1,2)
-}

getMostDP :: (Position, Position) -> Directions -> Position
getMostDP ((a, b), (c, d)) dir = case dir of 
    North -> if a < c then (a - 1, b) else (c - 1, b)
    South -> if a > c then (a + 1, b) else (c + 1, b)
    East -> if b > d then (a, b + 1) else (a, d + 1)
    _ -> if b < d then (a, b - 1) else (a, d - 1)

{-
    @lvl -> lvl on which to execute the check
    @((x, y), (a, b)) -> pair of positions to be checked

    Function that checks if the given positions within the bounds of the map
    or not. Used when moving
-}

isOutOfBounds :: Level -> (Position, Position) -> Bool
isOutOfBounds lvl ((x, y), (a, b)) = if (x < x1 || x > x2 || y < y1 || y > y2) || ((a < x1 || a > x2 || b < y1 || b > y2) && a /= -2  && b /= -2) then True else False
    where 
        ((x1, y1), (x2, y2)) = A.bounds $ matrix lvl

{- 
    To move into any direction (N E S W - clock wise) it's enough to add to the
    currrent position the directional offset

    You need to take into account if the block is in vertical position or 
    horizontal. There's 3 cases:
        - vertical 
        - horizontal, oriented from north to south on the map
        - horizontal, oriented from east to west on the map
        _ _ _ _ _ _         _ _ _ _ _ _
        _ _ # _ _ _         _ _ # # _ _ 
        _ _ # _ _ _         _ _ _ _ _ _ 
        _ _ _ _ _ _         _ _ _ _ _ _ 
         hnsCube1&2          hewCube1&2

    hnsCube - horizontal North South cube 
    hewCube - horizontal East West cube 
-}

justMoving :: Directions -> Level -> Level
justMoving dir lvl = if ((snd $ blck lvl) == naPosition) 
    then if isOutOfBounds lvl (vCube1, vCube2) then lvl {gs = -1} else lvl {blck = (vCube1, vCube2)}
    else if (fst $ fst $ blck lvl) == (fst $ snd $ blck lvl)
        then if isOutOfBounds lvl (hewCube1, hewCube2) then lvl {gs = -1} else lvl {blck = (hewCube1, hewCube2)}
        else if isOutOfBounds lvl (hnsCube1, hnsCube2) then lvl {gs = -1} else lvl {blck = (hnsCube1, hnsCube2)}
    where
        vCube1 = addPositions (fst $ blck lvl) (directionalOffset !! (getDirection dir))
        vCube2 = addPositions vCube1 (directionalOffset !! (getDirection dir))
        hewCube1 = if (dir == East) || (dir == West) 
            then getMostDP (blck lvl) dir
            else (addPositions (fst $ blck lvl) (directionalOffset !! (getDirection dir)))
        hewCube2 = if (dir == East) || (dir == West) 
            then naPosition else (addPositions (snd $ blck lvl) (directionalOffset !! (getDirection dir)))
        hnsCube1 = if (dir == North) || (dir == South) 
            then getMostDP (blck lvl) dir 
            else (addPositions (fst $ blck lvl) (directionalOffset !! (getDirection dir)))
        hnsCube2 = if (dir == North) || (dir == South) 
            then naPosition else (addPositions (snd $ blck lvl) (directionalOffset !! (getDirection dir)))

{-
    @dir -> the direction in which to move the block 
    @lvl -> lvl on which the move is happening

    Function that moves the block in a certain direction and also activates
    every block if need be 
-}

move :: Directions -> Level -> Level
move dir lvl = if (gs lvl) == 0 then foldl (\ x y -> activate y x) (justMoving dir lvl) activCells else lvl

{-
    Function that checks wheter the game is done or not.

    It returns true if the game keeps going and false if the game is won/lost.

    Used in Interactive.
-}

continueGame :: Level -> Bool
continueGame lvl = (gs lvl == 0)

{-
    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors lvl = filter ((== 0) . gs. snd) $ zip directionList (map (\ x -> move x lvl) directionList)

    isGoal lvl = (gs lvl == 1)

    -- Doar petru BONUS
    -- heuristic = undefined
    