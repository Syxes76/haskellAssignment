import Control.Concurrent (threadDelay)
import System.IO
import System.Exit
import System.Random
import Data.List
import System.Console.ANSI
import Control.DeepSeq
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game





-- dis iz da hud

( pictures [translate (-112.5) (263.5) $ color (greyN 0.75) $ rectangleSolid 695 54
                                                                                , translate (-450) 243 $ scale 0.15 0.15 $ Text "Nearest water : 10"
                                                                                , translate (-225) 243 $ scale 0.15 0.15 $ Text "Nearest portal : 10"
                                                                                , translate (0) 243 $ scale 0.15 0.15 $ Text "Nearest treasure : 10"
                                                                                , translate (-400) (270) $ scale 0.15 0.15 $ Text "Water units left : 15"
                                                                                , translate (-70) (270) $ scale 0.15 0.15 $ Text "Treasures found  : 15"
                                                                                , translate (-345) (250) $ rectangleWire 230 27
                                                                                , translate (-120) (250) $ rectangleWire 220 27
                                                                                , translate (112.5) (250) $ rectangleWire 245 27
                                                                                , translate (-286.5) (277) $ rectangleWire 347 27
                                                                                , translate (61) (277) $ rectangleWire 348 27
                                                                                ] )






















-- Line of Sight
lineOfSight = 4  

-- Max water capacity      
maxWaterLevel = 20

-- Water units refilled when walking over a water tile
waterRefill = 15
        
-- Random generator (with seed)
randGen = mkStdGen 1337

-- Tile generation parameters
treasureProb = 33
waterProb = 10
portalProb = 5
lavaProb = 15
adjLavaProb = 10

-- Depth-First-Search step limit (recommended value of 8-10)
maxPathLength = 10

-- These values control how the map is rendered
-- For a terminal with a resolution of 80x24, recommended values are :
-- renderWidth = 9
-- renderHeight = 4
-- The double of this value plus 1 represents the amount of tiles rendered horizontaly
renderWidth = 4
-- The double of this value plus 1 represents the amount of tiles rendered verticaly
renderHeight = 4

tileSizeInt :: Int
tileSizeInt = 25

tileSize :: Float
tileSize = fromIntegral tileSizeInt

-- Misc function for removing duplicates
-- taken from StackOverflow
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort


-- Player/State related functions

data State = State { xPos :: Int
                    , yPos :: Int
                    , waterLevel :: Int
                    , tilesLooted :: [(Int,Int)]
                    , tilesVisible :: [(Int,Int)]
                    , map :: [Tile]
                    , score :: Int}
    deriving Show

-- Moves player one tile in the specified direction and reduces water by one unit
movePlayerUp State { xPos = newXPos, yPos = newYPos, waterLevel = newWaterLevel} =
    if newYPos > 0
        then State { xPos = newXPos, yPos = newYPos - 1, waterLevel = newWaterLevel - 1 }
        else State { xPos = newXPos, yPos = newYPos, waterLevel = newWaterLevel }

movePlayerDown State { xPos = newXPos, yPos = newYPos, waterLevel = newWaterLevel } = 
    State { xPos = newXPos, yPos = newYPos + 1, waterLevel = newWaterLevel - 1 }

movePlayerLeft State { xPos = newXPos, yPos = newYPos, waterLevel = newWaterLevel } = 
    if newXPos > 0
        then State { xPos = newXPos - 1, yPos = newYPos, waterLevel = newWaterLevel - 1 }
        else State { xPos = newXPos, yPos = newYPos, waterLevel = newWaterLevel }

movePlayerRight State { xPos = newXPos, yPos = newYPos, waterLevel = newWaterLevel } = 
    State { xPos = newXPos + 1, yPos = newYPos, waterLevel = newWaterLevel - 1 }

-- Refills player's water by a constant amount of units
refillWater State { xPos = newXPos, yPos = newYPos, waterLevel = newWaterLevel } =
    State { xPos = newXPos, yPos = newYPos, waterLevel = minimum([maxWaterLevel,newWaterLevel+(waterRefill)]) }

-- Prints out the "status" section of the display with information such as water left, treasures found
-- and distance to nearby points of interest
renderState :: State -> Int -> [(Int,Int)] -> [Tile] -> IO ()
renderState state score tilesLooted map = do
    putStrLn    "==========================================================================="
    putStrLn     ("  Water units left : " ++ (show (waterLevel state)) ++ "   Treasures found  : " ++ (show score))
    if waterProb > 0
        then do
            let waterDist = getClosestTile (xPos state) (yPos state) 'w' tilesLooted map
            if waterDist == maxPathLength*2
                then putStr ("  Nearest water : " ++(show maxPathLength)++"+")
                else putStr ("  Nearest water : " ++ (show waterDist))
        else putStr ("  Nearest water : None")
    if portalProb > 0
        then do
            let portalDist = getClosestTile (xPos state) (yPos state) 'p' tilesLooted map
            if portalDist == maxPathLength*2
                then putStr ("   Nearest portal : " ++(show maxPathLength)++"+")
                else putStr ("  Nearest portal : " ++ (show portalDist))
        else putStr ("  Nearest portal : None")
    if treasureProb > 0
        then do
            let treasureDist = getClosestTile (xPos state) (yPos state) 'd' tilesLooted map
            if treasureDist == maxPathLength*2
                then putStrLn ("   Nearest treasure : " ++(show maxPathLength)++"+")
                else putStrLn ("  Nearest treasure : " ++ (show treasureDist))
        else putStrLn ("  Nearest treasure : None")
    putStrLn    "==========================================================================="


-- Map related functions

data Tile = Tile { tileType :: Char, hasTreasure :: Bool }
    deriving Show  

-- Randomly generates a tile type
randomTile :: Int -> Int -> Int -> Tile
randomTile x y num = do
    let randNum = (randomRs (1,100) (randGen) :: [Int]) !! num
    if randNum `seq` (randNum <= waterProb)
        then Tile { tileType = 'w', hasTreasure = False }
        else if (waterProb + portalProb ) `seq` (randNum <= (waterProb + portalProb ))
            then Tile { tileType = 'p', hasTreasure = False }
            else if (waterProb + portalProb + lavaProb) `seq` (randNum <= waterProb + portalProb + lavaProb)
                then Tile { tileType = 'l', hasTreasure = False }
                else do
                    let randNum = (num+1) `seq` (randomRs (1,100) (randGen) :: [Int]) !! (num+1)
                    if randNum `seq` (randNum <= treasureProb)
                        then Tile { tileType = 'd', hasTreasure = True }
                        else Tile { tileType = 'd', hasTreasure = False }

-- Randomly generates a tile type (adjacent to a lava tile)
randomTile' :: Int -> Int -> Int -> Tile
randomTile' x y num = do
    let randNum = (randomRs (1,100) (randGen) :: [Int]) !! num
    if randNum `seq` (randNum <= waterProb)
        then Tile { tileType = 'w', hasTreasure = False }
        else if (waterProb + portalProb ) `seq` (randNum <= (waterProb + portalProb ))
            then Tile { tileType = 'p', hasTreasure = False }
            else if (waterProb + portalProb + adjLavaProb) `seq` (randNum <= waterProb + portalProb + adjLavaProb)
                then Tile { tileType = 'l', hasTreasure = False }
                else do
                    let randNum = (num+1) `seq` (randomRs (1,100) (randGen) :: [Int]) !! (num+1)
                    if randNum `seq` (randNum <= treasureProb)
                        then Tile { tileType = 'd', hasTreasure = True }
                        else Tile { tileType = 'd', hasTreasure = False }

-- Randomly generates a complete tile
generateTile :: Int -> Int -> [Tile] -> Int -> Tile
generateTile x y map num
    | x > 0 && y > 0 = do
        let adjYTile = (y-1) `seq` getTile x (y-1) map
            adjXTile = (x-1) `seq` getTile (x-1) y map
        if adjXTile `seq` adjYTile `seq` ((tileType adjXTile) == 'l' || (tileType adjYTile) == 'l')
            then randomTile' x y num
            else randomTile x y num
    | x > 0 = do
        let adjXTile = (x-1) `seq` getTile (x-1) y map
        if adjXTile `seq` ((tileType adjXTile) == 'l')
            then randomTile' x y num
            else randomTile x y num
    | y > 0 = do
        let adjYTile = (y-1) `seq` getTile x (y-1) map
        if adjYTile `seq` ((tileType adjYTile) == 'l')
            then randomTile' x y num
            else randomTile x y num
    | otherwise = Tile { tileType = 'd', hasTreasure = False }

-- Generates an infinite map in the form of a matrix (it is however kept in memory as a list)
-- Starter function 
generateMatrix :: [Tile]
generateMatrix  = generateMatrix' 0 0 [] 1

generateMatrix' :: Int -> Int -> [Tile] -> Int -> [Tile]
generateMatrix' 0 0 map num = do
    let currTile = generateTile 0 0 map num 
    currTile `seq` ([currTile] ++ ((map ++ [currTile]) `seq` (num+2) `seq` (generateMatrix' 1 0 (map ++ [currTile]) (num+2))))
generateMatrix' x 0 map num = do
    let currTile = generateTile x 0 map num 
    currTile `seq` ([currTile] ++ ((map ++ [currTile]) `seq` (num+2) `seq` (generateMatrix' 0 x (map ++ [currTile]) (num+2))))
generateMatrix' 0 y map num = do
    let currTile = generateTile 0 y map num 
    currTile `seq` ([currTile] ++ ((map ++ [currTile]) `seq` (num+2) `seq` (generateMatrix' y 1 (map ++ [currTile]) (num+2))))
generateMatrix' x y map num 
    | x > y = do
        let currTile = generateTile x y map num 
        currTile `seq` ([currTile] ++ ((map ++ [currTile]) `seq` (num+2) `seq` (generateMatrix' y x (map ++ [currTile]) (num+2))))
    | x < y = do
        let currTile = generateTile x y map num 
        currTile `seq` ([currTile] ++ ((map ++ [currTile]) `seq` (num+2) `seq` (x+1) `seq` (generateMatrix' y (x+1) (map ++ [currTile]) (num+2))))
    | x == y = do
        let currTile = generateTile x y map num 
        currTile `seq` ([currTile] ++ ((map ++ [currTile]) `seq` (num+2) `seq` (x+1) `seq` (generateMatrix' (x+1) 0 (map ++ [currTile]) (num+2))))

-- Translates 2-dimensional coordinates into proper index values for searching tiles within the list of generated tiles
getTile :: Int -> Int -> [Tile] -> Tile
getTile 0 0 map = map!!0
getTile x 0 map = do
    if x > 0
        then (x*x) `deepseq` map!!(x*x)
        else Tile { tileType = 'x', hasTreasure = False }
getTile 0 y map = do
    if y > 0
        then ((y*y)+1) `deepseq` map!!((y*y)+1)
        else Tile { tileType = 'x', hasTreasure = False }
getTile x y map
    | x < 0 || y < 0 = Tile { tileType = 'x', hasTreasure = False }
    | x > y = ((x*x)+2*y) `deepseq` map!!((x*x)+2*y)
    | x < y = ((y*y)+1+2*x) `deepseq` map!!((y*y)+1+2*x)
    | x == y = (((x+1)*(y+1))-1) `deepseq` map!!(((x+1)*(y+1))-1)

-- Creates a line of sight radius and returns a list of all 'visible' tiles
-- Starter function
detectTiles :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
detectTiles x y tilesVisible = detectTiles' x y 0 0 tilesVisible

detectTiles' :: Int -> Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)]
detectTiles' x y nx ny tilesVisible
    | ny == (lineOfSight*2) = do
        if not (elem (x,(y+lineOfSight)) tilesVisible)
            then (y+lineOfSight) `seq` [(x,(y+lineOfSight))]
            else []
    | ny >= 0 && ny < lineOfSight = do
        if (nx <= ny)
            then ((x+nx) `seq` (y-(lineOfSight-ny)) `deepseq` [((x+nx),(y-(lineOfSight-ny)))]) ++ ((nx+1) `seq` detectTiles' x y (nx+1) ny tilesVisible)
            else [] ++ ((ny+1) `seq` (negate (ny+1)) `seq` detectTiles' x y (negate (ny+1)) (ny+1) tilesVisible)
    | ny >= lineOfSight && ny < (lineOfSight*2) = do
        if (nx <= (lineOfSight-(mod ny lineOfSight)))
            then ((x+nx) `seq` (y+(ny-lineOfSight)) `deepseq` [((x+nx),(y+(ny-lineOfSight)))]) ++ ((nx+1) `deepseq` detectTiles' x y (nx+1) ny tilesVisible)
            else [] ++ ((ny+1) `seq` (negate (lineOfSight-(mod (ny+1) lineOfSight))) `deepseq` detectTiles' x y (negate (lineOfSight-(mod (ny+1) lineOfSight))) (ny+1) tilesVisible)

-- Prints out the map
-- Starter function
renderMap :: Int -> Int -> [(Int,Int)] -> [Tile] -> Picture
renderMap x y tilesVisible map = (negate renderWidth) `deepseq` (negate renderHeight) `deepseq` pictures (renderMap' x y (negate renderWidth) (negate renderHeight) tilesVisible map)

renderMap' :: Int -> Int -> Int -> Int -> [(Int,Int)] -> [Tile] -> [Picture]
renderMap' x y nx ny tilesVisible map
    | ny > renderHeight = []
    | nx <= renderWidth = do
            if (x+nx) `seq` (y+ny) `seq` elem ((x+nx),(y+ny)) tilesVisible
                then do
                    let currTile = (x+nx) `seq` (y+ny) `seq` getTile (x+nx) (y+ny) map
                        currTileType = currTile `seq` tileType currTile
                    case currTileType of 
                        'd' ->  [translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ color yellow $ rectangleSolid tileSize tileSize, translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ rectangleWire tileSize tileSize] ++ renderMap' x y (nx+1) ny tilesVisible map
                        'l' ->  [translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ color red $ rectangleSolid tileSize tileSize, translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ rectangleWire tileSize tileSize] ++ renderMap' x y (nx+1) ny tilesVisible map
                        'p' ->  [translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ color azure $ rectangleSolid tileSize tileSize, translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ rectangleWire tileSize tileSize] ++ renderMap' x y (nx+1) ny tilesVisible map
                        'w' ->  [translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ color blue $ rectangleSolid tileSize tileSize, translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ rectangleWire tileSize tileSize] ++ renderMap' x y (nx+1) ny tilesVisible map
                        _   ->  [translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ color black $ rectangleSolid tileSize tileSize, translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ rectangleWire tileSize tileSize] ++ renderMap' x y (nx+1) ny tilesVisible map
                else [translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ color (greyN 0.33) $ rectangleSolid tileSize tileSize, translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ rectangleWire tileSize tileSize] ++ renderMap' x y (nx+1) ny tilesVisible map
    | otherwise = (negate renderWidth) `deepseq` (ny+1) `seq` renderMap' x y (negate renderWidth) (ny+1) tilesVisible map
            

-- Searches for a specific tile type using Depth-First-Search and returns the length of the shortest path found
-- Starter function
getClosestTile :: Int -> Int -> Char -> [(Int,Int)] -> [Tile] -> Int
getClosestTile sx sy targetTile tilesLooted map = getClosestTile'' sx sy targetTile 0 tilesLooted [] map

-- ... using lazy evaluation
getClosestTile' :: Int -> Int -> Char -> Int -> [(Int,Int)] -> [(Int,Int)] -> [Tile] -> Int
getClosestTile' sx sy targetTile steps tilesLooted pathTaken map
    | (sx < 0) || (sy < 0) || (steps > maxPathLength) || (elem (sx,sy) pathTaken) = (maxPathLength*2)
    | otherwise = do
        let currTile = getTile sx sy map
        if (targetTile == (tileType currTile) && (not (elem (sx,sy) tilesLooted))) && (((targetTile == 'd') && (hasTreasure currTile)) || (targetTile /= 'd'))
            then steps
            else do
                if (tileType currTile) /= 'l' && (tileType currTile) /= 'p'
                    then do
                        let allPaths = [(getClosestTile' (sx+1) sy targetTile (steps+1) tilesLooted (pathTaken++[(sx,sy)]) map)] 
                            allPaths' = allPaths ++ [(getClosestTile' sx (sy+1) targetTile (steps+1) tilesLooted (pathTaken++[(sx,sy)]) map)] 
                            allPaths'' = allPaths' ++ [(getClosestTile' (sx-1) sy targetTile (steps+1) tilesLooted (pathTaken++[(sx,sy)]) map)] 
                            allPaths''' = allPaths'' ++ [(getClosestTile' sx (sy-1) targetTile (steps+1) tilesLooted (pathTaken++[(sx,sy)]) map)]
                        minimum (allPaths''')
                    else (maxPathLength*2)

-- using strict evaluation
getClosestTile'' :: Int -> Int -> Char -> Int -> [(Int,Int)] -> [(Int,Int)] -> [Tile] -> Int
getClosestTile'' sx sy targetTile steps tilesLooted pathTaken map
    | (sx < 0) || (sy < 0) || (steps > maxPathLength) || (elem (sx,sy) pathTaken) = (maxPathLength*2)
    | otherwise = do
        let currTile = map `seq` getTile sx sy map
        if currTile `seq` ((targetTile == (tileType currTile) && (not (elem (sx,sy) tilesLooted))) && (((targetTile == 'd') && (hasTreasure currTile)) || (targetTile /= 'd')))
            then steps
            else do
                if (tileType currTile) `seq` ((tileType currTile) /= 'l' && (tileType currTile) /= 'p')
                    then do
                        let allPaths =  ((sx+1) `seq` (steps+1) `seq` (pathTaken++[(sx,sy)]) `seq` [(getClosestTile'' (sx+1) sy targetTile (steps+1) tilesLooted (pathTaken++[(sx,sy)]) map)]) 
                            allPaths' = allPaths `deepseq` (allPaths ++ ((sy+1) `seq` (steps+1) `seq` (pathTaken++[(sx,sy)]) `seq` [(getClosestTile'' sx (sy+1) targetTile (steps+1) tilesLooted (pathTaken++[(sx,sy)]) map)]))
                            allPaths'' = allPaths' `deepseq` (allPaths' ++ ((sx-1) `seq` (steps+1) `seq` (pathTaken++[(sx,sy)]) `seq` [(getClosestTile'' (sx-1) sy targetTile (steps+1) tilesLooted (pathTaken++[(sx,sy)]) map)]) )
                            allPaths''' = allPaths'' `deepseq` (allPaths'' ++ ((sy-1) `seq` (steps+1) `seq` (pathTaken++[(sx,sy)]) `seq` [(getClosestTile'' sx (sy-1) targetTile (steps+1) tilesLooted (pathTaken++[(sx,sy)]) map)]))
                        allPaths''' `deepseq` minimum (allPaths''')
                    else (maxPathLength*2)



handleInput :: Event -> State -> State
handleInput (EventKey k ks _ _) state = do
    threadDelay 2000
    let currX = xPos state
        currY = yPos state
        currTile = currX `seq` currY `seq` getTile currX currY map
        currTileType = currTile `seq` tileType currTile
    case currTileType of 
        'd' ->  do
            if (elem (currX,currY) tilesLooted) || not (hasTreasure currTile)
                then return()
                else do
                    let tilesLooted' = tilesLooted ++ [(currX,currY)]
                    tilesLooted' `deepseq` loop state tilesLooted' tilesVisible map (score+1)
        'l' ->  do
            print "YOU FELL IN LAVA ! GAME OVER !"
            exitWith ExitSuccess
        'p' ->  do
            putStr "YOU MADE IT OUT ALIVE ! YOUR SCORE WAS " 
            print score
            exitWith ExitSuccess
        'w' ->  do
            if elem (currX,currY) tilesLooted
                then return()
                else do
                    let tilesLooted' = tilesLooted ++ [(currX,currY)]
                        state' = refillWater state
                    state' `seq` tilesLooted' `deepseq` loop state' tilesLooted' tilesVisible map score
    if waterLevel state < 1
        then do
            print "YOU RAN OUT OF WATER ! GAME OVER !"
            exitWith ExitSuccess
        else do
            clearScreen 
            let newTilesVisible = tilesVisible ++ (detectTiles currX currY tilesVisible)
                tilesVisible' = newTilesVisible `deepseq` rmdups newTilesVisible
            tilesVisible' `deepseq` renderMap currX currY tilesVisible' map
            renderState state score tilesLooted map
            key <- getChar
            case key of
                'k' -> do 
                    print "YOU KILLED YOURSELF ! GAME OVER !"
                    exitWith ExitSuccess
                'w' -> do 
                    let state' = movePlayerUp state
                    state' `seq` loop state' tilesLooted tilesVisible' map score
                's' -> do 
                    let state' = movePlayerDown state
                    state' `seq` loop state' tilesLooted tilesVisible' map score
                'a' -> do 
                    let state' = movePlayerLeft state
                    state' `seq` loop state' tilesLooted tilesVisible' map score
                'd' -> do 
                    let state' = movePlayerRight state
                    state' `seq` loop state' tilesLooted tilesVisible' map score
                _   -> do
                    let state' = state
                    state' `seq` loop state' tilesLooted tilesVisible' map score


main = do
    if lineOfSight < 0 || maxWaterLevel < 0 || waterRefill < 0 || treasureProb < 0 || waterProb < 0 || portalProb < 0 || lavaProb < 0 || adjLavaProb < 0 || maxPathLength < 0 || renderWidth <= 0 || renderHeight <= 0 
        then do
            print "ERROR : negative parameters detected ! please provide only positive values for each parameter ! (renderWidth and renderHeight must be greater than 0 as well)"
            exitWith ExitSuccess
        else return ()
    if (waterProb + portalProb + lavaProb > 100) || (waterProb + portalProb + adjLavaProb > 100)
        then do
            print "ERROR : spawning probabilities sum is greater than 100 !"
            exitWith ExitSuccess
        else return ()
    let initialState = State {xPos = 0, yPos = 0, waterLevel = maxWaterLevel }
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    let initialMap =  randGen `seq` generateMatrix 
    initialState `seq` initialMap `seq` loop initialState [] [] initialMap 0
