import System.IO
import System.Exit
import System.Random
import Data.List
import Control.DeepSeq
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Monad
import Control.Parallel
import Control.Concurrent
import Control.Concurrent.STM

-- Line of Sight
lineOfSight = 4  

-- Max water capacity      
maxWaterLevel = 20

-- Water units refilled when walking over a water tile
waterRefill = 15

maxWormLength = 5

-- Random generator (with seed)
randGen = mkStdGen 1337

-- Tile generation parameters
treasureProb = 33
waterProb = 10
portalProb = 5
lavaProb = 15
adjLavaProb = 10
wormProb = 33

-- Depth-First-Search step limit (recommended value of 8-10)
maxPathLength = 10

-- These values control how the map is rendered
-- For a terminal with a resolution of 80x24, recommended values are :
-- renderWidth = 9
-- renderHeight = 4
-- The double of this value plus 1 represents the amount of tiles rendered horizontaly
renderWidth = 19
-- The double of this value plus 1 represents the amount of tiles rendered verticaly
renderHeight = 9

tileSizeInt :: Int
tileSizeInt = 25

tileSize :: Float
tileSize = fromIntegral tileSizeInt

xOffset = (-50)

yOffset = (8)

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
                    , gameMap :: [Tile]
                    , score :: Int
                    , wormPaths :: [[(Int,Int)]]
                    , gameMode :: Int
                    , stepsTaken :: Int
                    , availableThreads :: [Int]
                    , threadCount :: Int
                    , gameStatus :: Int
                    , lastKeyStroke :: Key }

-- Moves player one tile in the specified direction and reduces water by one unit
movePlayerUp :: TVar State -> IO ()
movePlayerUp gameState = do
    state <- readTVarIO gameState
    if (yPos state) > 0
        then do
            newState <- movePlayer (state { yPos = (yPos state) - 1, waterLevel = (waterLevel state) - 1 })
            atomically $ writeTVar gameState (newState {stepsTaken = (stepsTaken newState)+1})
            generateWorm gameState
            return ()
        else return ()

movePlayerDown :: TVar State -> IO ()
movePlayerDown gameState = do
    state <- readTVarIO gameState
    newState <- movePlayer (state { yPos = (yPos state) + 1, waterLevel = (waterLevel state) - 1 })
    atomically $ writeTVar gameState (newState {stepsTaken = (stepsTaken newState)+1})
    generateWorm gameState
    return ()

movePlayerLeft :: TVar State -> IO ()
movePlayerLeft gameState = do
    state <- readTVarIO gameState
    if (xPos state) > 0
        then do
            newState <- movePlayer (state { xPos = (xPos state) - 1, waterLevel = (waterLevel state) - 1 })
            atomically $ writeTVar gameState (newState {stepsTaken = (stepsTaken newState)+1})
            generateWorm gameState
            return ()
        else return ()

movePlayerRight :: TVar State -> IO ()
movePlayerRight gameState = do
    state <- readTVarIO gameState
    newState <- movePlayer (state { xPos = (xPos state) + 1, waterLevel = (waterLevel state) - 1 })
    atomically $ writeTVar gameState (newState {stepsTaken = (stepsTaken newState)+1})
    generateWorm gameState
    return ()

-- Refills player's water by a constant amount of units
refillWater :: State -> IO State
refillWater state = 
    return (state { waterLevel = minimum([maxWaterLevel,(waterLevel state)+(waterRefill)]) })

-- Handles flow of the game as the player moves
movePlayer :: State -> IO State
movePlayer state =
    if (waterLevel state) < 0
        then do
            return (state {gameMode = -1})
        else do
            let newTilesVisible = (tilesVisible state) ++ (detectTiles currX currY (tilesVisible state))
                tilesVisible' = newTilesVisible `deepseq` rmdups newTilesVisible
                currX = xPos state
                currY = yPos state
                currTile = currX `seq` currY `seq` getTile currX currY (gameMap state)
                currTileType = currTile `seq` tileType currTile
            case currTileType of 
                'd' ->  do
                    if elem (currX,currY) (filter (\x -> if (fst x) >= 0 && (snd x) >= 0 then True else False) (concat (wormPaths state)))
                        then return (state {gameMode = -4})
                        else if (elem (currX,currY) (tilesLooted state)) || not (hasTreasure currTile)
                            then return (state { tilesVisible = tilesVisible' })
                            else return (state { tilesLooted = ((tilesLooted state) ++ [(currX,currY)]), tilesVisible = tilesVisible', score = (score state) + 1 })
                'l' ->  do
                    return (state {gameMode = -2})
                'p' ->  do
                    return (state {gameMode = -3})
                'w' ->  do
                    if elem (currX,currY) (tilesLooted state)
                        then return (state {tilesVisible = tilesVisible'})
                        else refillWater (state { tilesLooted = ((tilesLooted state) ++ [(currX,currY)]), tilesVisible = tilesVisible' })


-- gameMap related functions

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
generateTile x y gameMap num
    | x > 0 && y > 0 = do
        let adjYTile = (y-1) `seq` getTile x (y-1) gameMap
            adjXTile = (x-1) `seq` getTile (x-1) y gameMap
        if adjXTile `seq` adjYTile `seq` ((tileType adjXTile) == 'l' || (tileType adjYTile) == 'l')
            then randomTile' x y num
            else randomTile x y num
    | x > 0 = do
        let adjXTile = (x-1) `seq` getTile (x-1) y gameMap
        if adjXTile `seq` ((tileType adjXTile) == 'l')
            then randomTile' x y num
            else randomTile x y num
    | y > 0 = do
        let adjYTile = (y-1) `seq` getTile x (y-1) gameMap
        if adjYTile `seq` ((tileType adjYTile) == 'l')
            then randomTile' x y num
            else randomTile x y num
    | otherwise = Tile { tileType = 'd', hasTreasure = False }

-- Generates an infinite gameMap in the form of a matrix (it is however kept in memory as a list)
-- Starter function 
generateMatrix :: [Tile]
generateMatrix  = generateMatrix' 0 0 [] 1

generateMatrix' :: Int -> Int -> [Tile] -> Int -> [Tile]
generateMatrix' 0 0 gameMap num = do
    let currTile = generateTile 0 0 gameMap num 
    currTile `seq` ([currTile] ++ ((gameMap ++ [currTile]) `seq` (num+2) `seq` (generateMatrix' 1 0 (gameMap ++ [currTile]) (num+2))))
generateMatrix' x 0 gameMap num = do
    let currTile = generateTile x 0 gameMap num 
    currTile `seq` ([currTile] ++ ((gameMap ++ [currTile]) `seq` (num+2) `seq` (generateMatrix' 0 x (gameMap ++ [currTile]) (num+2))))
generateMatrix' 0 y gameMap num = do
    let currTile = generateTile 0 y gameMap num 
    currTile `seq` ([currTile] ++ ((gameMap ++ [currTile]) `seq` (num+2) `seq` (generateMatrix' y 1 (gameMap ++ [currTile]) (num+2))))
generateMatrix' x y gameMap num 
    | x > y = do
        let currTile = generateTile x y gameMap num 
        currTile `seq` ([currTile] ++ ((gameMap ++ [currTile]) `seq` (num+2) `seq` (generateMatrix' y x (gameMap ++ [currTile]) (num+2))))
    | x < y = do
        let currTile = generateTile x y gameMap num 
        currTile `seq` ([currTile] ++ ((gameMap ++ [currTile]) `seq` (num+2) `seq` (x+1) `seq` (generateMatrix' y (x+1) (gameMap ++ [currTile]) (num+2))))
    | x == y = do
        let currTile = generateTile x y gameMap num 
        currTile `seq` ([currTile] ++ ((gameMap ++ [currTile]) `seq` (num+2) `seq` (x+1) `seq` (generateMatrix' (x+1) 0 (gameMap ++ [currTile]) (num+2))))

-- Translates 2-dimensional coordinates into proper index values for searching tiles within the list of generated tiles
getTile :: Int -> Int -> [Tile] -> Tile
getTile 0 0 gameMap = gameMap!!0
getTile x 0 gameMap = do
    if x > 0
        then (x*x) `deepseq` gameMap!!(x*x)
        else Tile { tileType = 'x', hasTreasure = False }
getTile 0 y gameMap = do
    if y > 0
        then ((y*y)+1) `deepseq` gameMap!!((y*y)+1)
        else Tile { tileType = 'x', hasTreasure = False }
getTile x y gameMap
    | x < 0 || y < 0 = Tile { tileType = 'x', hasTreasure = False }
    | x > y = ((x*x)+2*y) `deepseq` gameMap!!((x*x)+2*y)
    | x < y = ((y*y)+1+2*x) `deepseq` gameMap!!((y*y)+1+2*x)
    | x == y = (((x+1)*(y+1))-1) `deepseq` gameMap!!(((x+1)*(y+1))-1)

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

-- Searches for a specific tile type using Depth-First-Search and returns the length of the shortest path found
-- Starter function
getClosestTile :: Int -> Int -> Char -> [(Int,Int)] -> [Tile] -> String
getClosestTile sx sy targetTile tilesLooted gameMap = do 
    if (targetTile == 'w' && waterProb > 0)
        then do
            let waterDist = getClosestTile'' sx sy targetTile 0 tilesLooted [] gameMap
            if waterDist == maxPathLength*2
                then ((show maxPathLength)++"+")
                else (show waterDist)
        else if (targetTile == 'p' && portalProb > 0)
            then do
                let portalDist = getClosestTile'' sx sy targetTile 0 tilesLooted [] gameMap
                if portalDist == maxPathLength*2
                    then ((show maxPathLength)++"+")
                    else (show portalDist)
            else if (targetTile == 'd' && treasureProb > 0) 
                then do
                    let treasureDist = getClosestTile'' sx sy targetTile 0 tilesLooted [] gameMap
                    if treasureDist == maxPathLength*2
                        then ((show maxPathLength)++"+")
                        else (show treasureDist)
                else "None"

-- ... using lazy evaluation
getClosestTile' :: Int -> Int -> Char -> Int -> [(Int,Int)] -> [(Int,Int)] -> [Tile] -> Int
getClosestTile' sx sy targetTile steps tilesLooted pathTaken gameMap
    | (sx < 0) || (sy < 0) || (steps > maxPathLength) || (elem (sx,sy) pathTaken) = (maxPathLength*2)
    | otherwise = do
        let currTile = getTile sx sy gameMap
        if (targetTile == (tileType currTile) && (not (elem (sx,sy) tilesLooted))) && (((targetTile == 'd') && (hasTreasure currTile)) || (targetTile /= 'd'))
            then steps
            else do
                if (tileType currTile) /= 'l' && (tileType currTile) /= 'p'
                    then do
                        let allPaths = [(getClosestTile' (sx+1) sy targetTile (steps+1) tilesLooted (pathTaken++[(sx,sy)]) gameMap)] 
                            allPaths' = allPaths ++ [(getClosestTile' sx (sy+1) targetTile (steps+1) tilesLooted (pathTaken++[(sx,sy)]) gameMap)] 
                            allPaths'' = allPaths' ++ [(getClosestTile' (sx-1) sy targetTile (steps+1) tilesLooted (pathTaken++[(sx,sy)]) gameMap)] 
                            allPaths''' = allPaths'' ++ [(getClosestTile' sx (sy-1) targetTile (steps+1) tilesLooted (pathTaken++[(sx,sy)]) gameMap)]
                        minimum (allPaths''')
                    else (maxPathLength*2)

-- using strict evaluation
getClosestTile'' :: Int -> Int -> Char -> Int -> [(Int,Int)] -> [(Int,Int)] -> [Tile] -> Int
getClosestTile'' sx sy targetTile steps tilesLooted pathTaken gameMap
    | (sx < 0) || (sy < 0) || (steps > maxPathLength) || (elem (sx,sy) pathTaken) = (maxPathLength*2)
    | otherwise = do
        let currTile = gameMap `seq` getTile sx sy gameMap
        if currTile `seq` ((targetTile == (tileType currTile) && (not (elem (sx,sy) tilesLooted))) && (((targetTile == 'd') && (hasTreasure currTile)) || (targetTile /= 'd')))
            then steps
            else do
                if (tileType currTile) `seq` ((tileType currTile) /= 'l' && (tileType currTile) /= 'p')
                    then do
                        let allPaths =  ((sx+1) `seq` (steps+1) `seq` (pathTaken++[(sx,sy)]) `seq` [(getClosestTile'' (sx+1) sy targetTile (steps+1) tilesLooted (pathTaken++[(sx,sy)]) gameMap)]) 
                            allPaths' = allPaths `deepseq` (allPaths ++ ((sy+1) `seq` (steps+1) `seq` (pathTaken++[(sx,sy)]) `seq` [(getClosestTile'' sx (sy+1) targetTile (steps+1) tilesLooted (pathTaken++[(sx,sy)]) gameMap)]))
                            allPaths'' = allPaths' `deepseq` (allPaths' ++ ((sx-1) `seq` (steps+1) `seq` (pathTaken++[(sx,sy)]) `seq` [(getClosestTile'' (sx-1) sy targetTile (steps+1) tilesLooted (pathTaken++[(sx,sy)]) gameMap)]) )
                            allPaths''' = allPaths'' `deepseq` (allPaths'' ++ ((sy-1) `seq` (steps+1) `seq` (pathTaken++[(sx,sy)]) `seq` [(getClosestTile'' sx (sy-1) targetTile (steps+1) tilesLooted (pathTaken++[(sx,sy)]) gameMap)]))
                        allPaths''' `deepseq` minimum (allPaths''')
                    else (maxPathLength*2)

generateWorm :: TVar State -> IO ()
generateWorm gameState = do
    state <- readTVarIO gameState
    let randNum = (randomRs (1,100) (randGen) :: [Int]) !! (stepsTaken state)
    if randNum `seq` (randNum <= wormProb) 
        then do
            if (length (availableThreads state)) > 0
                then do 
                    let newCoordPool = filter (\x -> getValidWormTiles (-2,-2) x (xPos state, yPos state) (wormPaths state) [] (tilesLooted state) (gameMap state)) (detectTiles (xPos state) (yPos state) (tilesVisible state))
                    if ((length newCoordPool) > 0)
                        then do
                            let chosenCoords = (newCoordPool!!(randNum `mod` (length newCoordPool)))
                                chosenThread = head (availableThreads state)
                            atomically $ writeTVar gameState (state {
                                                            wormPaths = (take (chosenThread) (wormPaths state)) ++ [[(-1,-1), chosenCoords]] ++ (drop (chosenThread+1) (wormPaths state))
                                                            , availableThreads = tail (availableThreads state)
                                                            , gameStatus = ((threadCount state)-(length (availableThreads state))+1) })
                            return ()
                        else atomically $ writeTVar gameState (state {gameStatus = (threadCount state)})
                else do
                    (createForkedThreads gameState 1) >> (generateWorm gameState)
        else atomically $ writeTVar gameState (state {gameStatus = ((threadCount state)-(length (availableThreads state)))}) 

controlWorm :: TVar State -> Int -> IO ()
controlWorm gameState threadIndex = do
    state <- readTVarIO gameState
    if (gameStatus state) > 0 
        && ((head ((wormPaths state)!!threadIndex)) /= (-1,-1) || ((head ((wormPaths state)!!threadIndex)) == (-1,-1) && (length ((wormPaths state)!!threadIndex)) > 1))
        then do
            if (head ((wormPaths state)!!threadIndex)) /= (-1,-1)
                then moveWorm gameState threadIndex
                else atomically $ do 
                        newState <- readTVar gameState
                        writeTVar gameState (newState {
                            wormPaths = (take (threadIndex) (wormPaths newState)) ++ [tail ((wormPaths newState)!!threadIndex)] ++ (drop (threadIndex+1) (wormPaths newState))
                            , gameStatus = (gameStatus newState)-1 })
            checkLock gameState 0
            controlWorm gameState threadIndex
        else do
            threadDelay 90000
            controlWorm gameState threadIndex

moveWorm :: TVar State -> Int -> IO ()
moveWorm gameState threadIndex = do
    state <- readTVarIO gameState
    let currWormPath = ((wormPaths state)!!threadIndex)
    if (last currWormPath) /= (-1,-1)
        then do
            let newCoordPool = filter (\x -> 
                                    getValidWormTiles (head currWormPath) x (xPos state, yPos state) (wormPaths state) currWormPath (tilesLooted state) (gameMap state))
                                    [((fst (head currWormPath))+1,(snd (head currWormPath)))
                                    ,((fst (head currWormPath))-1,(snd (head currWormPath)))
                                    ,((fst (head currWormPath)),(snd (head currWormPath))+1)
                                    ,((fst (head currWormPath)),(snd (head currWormPath))-1)]
                randNum = (randomRs (1,100) (randGen) :: [Int]) !! (stepsTaken state)
            if (length newCoordPool) > 0 && (length currWormPath) < maxWormLength
                then do
                    let newCoord = head (sortBy (\a b -> compare (getDistance a (xPos state, yPos state)) (getDistance b (xPos state, yPos state))) newCoordPool)
                    atomically $ do 
                        newState <- readTVar gameState
                        writeTVar gameState (newState {
                            wormPaths = (take (threadIndex) (wormPaths newState)) ++ [[newCoord] ++ ((wormPaths newState)!!threadIndex)] ++ (drop (threadIndex+1) (wormPaths newState))
                            , gameStatus = (gameStatus newState)-1 })
                else if (length currWormPath) > 1
                    then atomically $ do 
                            newState <- readTVar gameState
                            writeTVar gameState (newState {
                                wormPaths = (take (threadIndex) (wormPaths newState)) ++ [(tail (reverse ((wormPaths newState)!!threadIndex))) ++ [(-1,-1)]] ++ (drop (threadIndex+1) (wormPaths newState))
                                , gameStatus = (gameStatus newState)-1 })
                    else atomically $ do 
                            newState <- readTVar gameState
                            writeTVar gameState (newState {
                                wormPaths = (take (threadIndex) (wormPaths newState)) ++ [[(-1,-1)]] ++ (drop (threadIndex+1) (wormPaths newState))
                                , availableThreads = (availableThreads newState) ++ [threadIndex]
                                , gameStatus = (gameStatus newState)-1 })
        else if (head (tail ((wormPaths state)!!threadIndex))) /= (-1,-1)
            then atomically $ do 
                    newState <- readTVar gameState
                    writeTVar gameState (newState {
                        wormPaths = (take (threadIndex) (wormPaths newState)) ++ [tail ((wormPaths newState)!!threadIndex)] ++ (drop (threadIndex+1) (wormPaths newState))
                        , gameStatus = (gameStatus newState)-1 })
            else atomically $ do 
                    newState <- readTVar gameState
                    writeTVar gameState (newState {
                        wormPaths = (take (threadIndex) (wormPaths newState)) ++ [[(-1,-1)]] ++ (drop (threadIndex+1) (wormPaths newState))
                        , availableThreads = (availableThreads newState) ++ [threadIndex]
                        , gameStatus = (gameStatus newState)-1 })


getDistance :: (Int,Int) -> (Int,Int) -> Float
getDistance startCoords endCoords = abs(sqrt(fromIntegral ((((fst endCoords) - (fst startCoords))^2)+(((snd endCoords) - (snd startCoords))^2))))

getValidWormTiles :: (Int,Int) -> (Int,Int) -> (Int,Int) -> [[(Int,Int)]] -> [(Int,Int)] -> [(Int,Int)] -> [Tile] -> Bool
getValidWormTiles headCoords coords playerCoords allWormPaths currWormPath tilesLooted gameMap = do
    if (not (elem coords ((getProjectedWormBodyTiles headCoords allWormPaths) ++ (concat allWormPaths))))
        && (not ((fst coords) < 0 || (snd coords) < 0))
        && (not ((fst playerCoords) == (fst coords) && (snd playerCoords) == (snd coords)))
        then do
            let currTile = getTile (fst coords) (snd coords) gameMap
                currTileType = currTile `seq` tileType currTile
            if (currTileType == 'd' && (not (hasTreasure currTile) || elem coords tilesLooted))
                then True
                else False
        else False

getProjectedWormBodyTiles :: (Int,Int) -> [[(Int,Int)]] -> [(Int,Int)]
getProjectedWormBodyTiles coords allWormPaths
    | (length allWormPaths) > 0 = do
        let currWormPath = head allWormPaths
        if ((head currWormPath) /= coords && (head currWormPath) /= (-1,-1))
            then [((fst (head currWormPath))+1,(snd (head currWormPath)))
                 ,((fst (head currWormPath))-1,(snd (head currWormPath)))
                 ,((fst (head currWormPath)),(snd (head currWormPath))+1)
                 ,((fst (head currWormPath)),(snd (head currWormPath))-1)] ++ (getProjectedWormBodyTiles coords (tail allWormPaths))
            else (getProjectedWormBodyTiles coords (tail allWormPaths))
    | otherwise = []

-- Prints out the map and HUD
-- Starter function
renderWindow :: TVar State -> IO Picture
renderWindow gameState = do
    checkLock gameState 0
    state <- readTVarIO gameState
    return ((negate renderWidth) `deepseq` (negate renderHeight) `deepseq` pictures ((renderState state) ++ (renderGameMap (xPos state) (yPos state) (negate renderWidth) (negate renderHeight) (tilesVisible state) (gameMap state)) ++ (renderPlayer state) ++ (renderWorms (xPos state) (yPos state) (negate renderWidth) (negate renderHeight) (tilesVisible state) (wormPaths state)) ++ (renderGameOver state)))

-- Prints out the "status" section of the display with information such as water left, treasures found
-- and distance to nearby points of interest
renderState :: State -> [Picture]
renderState state = 
    [translate (-112.5+xOffset) (263.5+yOffset) $ color (greyN 0.75) $ rectangleSolid 695 54
    , translate (-450+xOffset) (243+yOffset) $ scale 0.15 0.15 $ Text ("Nearest water : " ++ getClosestTile (xPos state) (yPos state) 'w' (tilesLooted state) (gameMap state))
    , translate (-225+xOffset) (243+yOffset) $ scale 0.15 0.15 $ Text ("Nearest portal : " ++ getClosestTile (xPos state) (yPos state) 'p' (tilesLooted state) (gameMap state))
    , translate (0+xOffset) (243+yOffset) $ scale 0.15 0.15 $ Text ("Nearest treasure : " ++ getClosestTile (xPos state) (yPos state) 'd' (tilesLooted state) (gameMap state))
    , translate (-400+xOffset) (270+yOffset) $ scale 0.15 0.15 $ Text ("Water units left : " ++ (if ((waterLevel state) == 0 || (waterLevel state) == (-1)) then "Dry" else show (waterLevel state)))
    , translate (-70+xOffset) (270+yOffset) $ scale 0.15 0.15 $ Text ("Treasures found  : " ++ (show (score state)))
    , translate (-345+xOffset) (250+yOffset) $ rectangleWire 230 27
    , translate (-120+xOffset) (250+yOffset) $ rectangleWire 220 27
    , translate (112.5+xOffset) (250+yOffset) $ rectangleWire 245 27
    , translate (-286.5+xOffset) (277+yOffset) $ rectangleWire 347 27
    , translate (61+xOffset) (277+yOffset) $ rectangleWire 348 27
    ]

renderGameMap :: Int -> Int -> Int -> Int -> [(Int,Int)] -> [Tile] -> [Picture]
renderGameMap x y nx ny tilesVisible gameMap
    | ny > renderHeight = []
    | nx <= renderWidth = do
        if (x+nx) `seq` (y+ny) `seq` elem ((x+nx),(y+ny)) tilesVisible
            then do
                let currTile = (x+nx) `seq` (y+ny) `seq` getTile (x+nx) (y+ny) gameMap
                    currTileType = currTile `seq` tileType currTile
                case currTileType of 
                    'd' ->  [translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ color yellow $ rectangleSolid tileSize tileSize, translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ rectangleWire tileSize tileSize] ++ renderGameMap x y (nx+1) ny tilesVisible gameMap
                    'l' ->  [translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ color red $ rectangleSolid tileSize tileSize, translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ rectangleWire tileSize tileSize] ++ renderGameMap x y (nx+1) ny tilesVisible gameMap
                    'p' ->  [translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ color azure $ rectangleSolid tileSize tileSize, translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ rectangleWire tileSize tileSize] ++ renderGameMap x y (nx+1) ny tilesVisible gameMap
                    'w' ->  [translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ color blue $ rectangleSolid tileSize tileSize, translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ rectangleWire tileSize tileSize] ++ renderGameMap x y (nx+1) ny tilesVisible gameMap
                    _   ->  [translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ color black $ rectangleSolid tileSize tileSize, translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ rectangleWire tileSize tileSize] ++ renderGameMap x y (nx+1) ny tilesVisible gameMap
            else [translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ color black $ rectangleSolid tileSize tileSize, translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ rectangleWire tileSize tileSize] ++ renderGameMap x y (nx+1) ny tilesVisible gameMap
    | otherwise = (negate renderWidth) `deepseq` (ny+1) `seq` renderGameMap x y (negate renderWidth) (ny+1) tilesVisible gameMap

renderPlayer :: State -> [Picture]
renderPlayer state = [color green $ circleSolid 9
                        , circle 9
                        , translate (-4) (-5) $ scale 0.1 0.1 $ Text "P"
                        ]

renderWorms :: Int -> Int -> Int -> Int -> [(Int,Int)] -> [[(Int,Int)]] -> [Picture]
renderWorms x y nx ny tilesVisible allWormPaths
    | ny > renderHeight = []
    | nx <= renderWidth = do
        if (x+nx) `seq` (y+ny) `seq` elem ((x+nx),(y+ny)) tilesVisible
            then do
                if elem ((x+nx),(y+ny)) (filter (\x -> if (fst x) >= 0 && (snd x) >= 0 then True else False) (concat allWormPaths))
                    then if (isWormHead ((x+nx),(y+ny)) allWormPaths)
                        then [translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ color red $ circleSolid 12
                            , translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ circle 12
                            , translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ circleSolid 7
                            ] ++ renderWorms x y (nx+1) ny tilesVisible allWormPaths
                        else [translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ color red $ circleSolid 11
                            , translate (fromIntegral nx*tileSize) (-(fromIntegral ny*tileSize)) $ circle 11
                            ] ++ renderWorms x y (nx+1) ny tilesVisible allWormPaths
                    else (nx+1) `seq` renderWorms x y (nx+1) ny tilesVisible allWormPaths
            else (nx+1) `seq` renderWorms x y (nx+1) ny tilesVisible allWormPaths
    | otherwise = (negate renderWidth) `deepseq` (ny+1) `seq` renderWorms x y (negate renderWidth) (ny+1) tilesVisible allWormPaths

isWormHead :: (Int,Int) -> [[(Int,Int)]] -> Bool
isWormHead coords allWormPaths
    | (length allWormPaths) > 0 = if ((head (head allWormPaths)) == coords && (last (head allWormPaths)) /= (-1,-1)) 
                                  || ((head (head allWormPaths)) == (-1,-1) && (last (head allWormPaths)) == coords)
                                  then True else isWormHead coords (tail allWormPaths)
    | otherwise = False

renderGameOver :: State -> [Picture]
renderGameOver state
    | (gameMode state == -1) = [color (greyN 0.44) $ rectangleSolid 900 500
                                , rectangleWire 900 500
                                , translate (-300) (50) $ color red $ scale 0.7 0.7 $ Text "GAME OVER !"
                                , translate (-340) (-50) $ color red $ scale 0.4 0.7 $ Text "YOU RAN OUT OF WATER !"
                                ]
    | (gameMode state == -2) = [color (greyN 0.44) $ rectangleSolid 900 500
                                , rectangleWire 900 500
                                , translate (-300) (50) $ color red $ scale 0.7 0.7 $ Text "GAME OVER !"
                                , translate (-260) (-50) $ color red $ scale 0.4 0.7 $ Text "YOU FELL IN LAVA !"
                                ]
    | (gameMode state == -3) = [color (greyN 0.44) $ rectangleSolid 900 500
                                , rectangleWire 900 500
                                , translate (-300) (50) $ color green $ scale 0.7 0.7 $ Text "SUCCESS !"
                                , translate (-380) (-50) $ color green $ scale 0.4 0.7 $ Text "YOU MANAGED TO ESCAPE !"
                                ]
    | (gameMode state == -4) = [color (greyN 0.44) $ rectangleSolid 900 500
                                , rectangleWire 900 500
                                , translate (-300) (50) $ color red $ scale 0.7 0.7 $ Text "GAME OVER !"
                                , translate (-300) (-50) $ color red $ scale 0.4 0.7 $ Text "YOU GOT SQUASHED"
                                , translate (-260) (-150) $ color red $ scale 0.4 0.7 $ Text "BY A WORM !"
                                ]
    | (gameMode state == -5) = [color (greyN 0.44) $ rectangleSolid 900 500
                                , rectangleWire 900 500
                                , translate (-300) (50) $ color red $ scale 0.7 0.7 $ Text "GAME OVER !"
                                , translate (-300) (-50) $ color red $ scale 0.4 0.7 $ Text "YOU KILLED YOURSELF !"
                                ]
    | otherwise = []

checkLock :: TVar State -> Int -> IO ()
checkLock gameState cond = do
    state <- readTVarIO gameState
    if ((gameStatus state) == cond)
        then return ()
        else do
            threadDelay 3000
            checkLock gameState cond

createForkedThreads :: TVar State -> Int -> IO ()
createForkedThreads gameState numThreads
    | wormProb > 0 && (numThreads > 0) = do
        state <- readTVarIO gameState
        forkIO $ controlWorm gameState (threadCount state)
        atomically $ writeTVar gameState (state {wormPaths = ((wormPaths state) ++ [[(-1,-1)]])
                                                , availableThreads = (availableThreads state) ++ [(threadCount state)]
                                                , threadCount = (threadCount state)+1})
        print (threadCount state)
        createForkedThreads gameState (numThreads-1)
    | otherwise = return ()

resetWormThreads :: [[(Int,Int)]] -> [[(Int,Int)]]
resetWormThreads allWormPaths
    | (length allWormPaths) > 0 = [[(-1,-1)]] ++ resetWormThreads (tail allWormPaths)
    | otherwise = []
    

getInput :: Event -> TVar State -> IO (TVar State)
getInput (EventKey key keyState _ _) gameState = do
    state <- readTVarIO gameState
    if (keyState == Down)
        then if ((gameMode state) == 0)
            then do
                case key of
                    (Char 'k') -> do 
                                atomically $ do
                                                newState <- readTVar gameState 
                                                writeTVar gameState (newState {lastKeyStroke = (Char 'k')})
                                return gameState
                    (Char 'w') -> do
                                atomically $do
                                                newState <- readTVar gameState 
                                                writeTVar gameState (newState {lastKeyStroke = (Char 'w')})
                                return gameState
                    (Char 's') -> do
                                atomically $ do
                                                newState <- readTVar gameState 
                                                writeTVar gameState (newState {lastKeyStroke = (Char 's')})
                                return gameState
                    (Char 'a') -> do
                                atomically $ do
                                                newState <- readTVar gameState 
                                                writeTVar gameState (newState {lastKeyStroke = (Char 'a')})
                                return gameState
                    (Char 'd') -> do
                                atomically $ do
                                                newState <- readTVar gameState 
                                                writeTVar gameState (newState {lastKeyStroke = (Char 'd')})
                                return gameState
                    _          -> do
                                atomically $ do
                                                newState <- readTVar gameState 
                                                writeTVar gameState (newState {lastKeyStroke = (Char '_')})
                                return gameState
            else if ((gameMode state) < 0) && ((gameMode state) >= -5)
                then do
                    atomically $ writeTVar gameState (state {xPos = 0
                                                            , yPos = 0
                                                            , waterLevel = maxWaterLevel
                                                            , tilesLooted = []
                                                            , tilesVisible = rmdups (detectTiles 0 0 [])
                                                            , score = 0
                                                            , wormPaths = resetWormThreads (wormPaths state)
                                                            , gameMode = 0
                                                            , stepsTaken = 0
                                                            , availableThreads = [0..((threadCount state)-1)]
                                                            , gameStatus = 0 })
                    return gameState
                else return gameState
        else return gameState
getInput _ gameState = return gameState

processInput :: Float -> TVar State -> IO (TVar State)
processInput _ gameState = do
    state <- readTVarIO gameState
    let key = (lastKeyStroke state)
    if ((gameMode state) == 0)
        then
            case key of
                (Char 'k') -> do 
                            atomically $do
                                            newState <- readTVar gameState 
                                            writeTVar gameState (newState {gameMode = -5, lastKeyStroke = (Char '_')})
                            return gameState
                (Char 'w') -> do
                            movePlayerUp gameState
                            newState <- readTVarIO gameState
                            checkLock gameState 0
                            atomically $ do
                                            newState <- readTVar gameState 
                                            writeTVar gameState (newState {lastKeyStroke = (Char '_')})
                            return gameState
                (Char 's') -> do
                            movePlayerDown gameState
                            newState <- readTVarIO gameState
                            checkLock gameState 0
                            atomically $ do
                                            newState <- readTVar gameState 
                                            writeTVar gameState (newState {lastKeyStroke = (Char '_')})
                            return gameState
                (Char 'a') -> do
                            movePlayerLeft gameState
                            newState <- readTVarIO gameState
                            checkLock gameState 0
                            atomically $ do
                                            newState <- readTVar gameState 
                                            writeTVar gameState (newState {lastKeyStroke = (Char '_')})
                            return gameState
                (Char 'd') -> do
                            movePlayerRight gameState
                            newState <- readTVarIO gameState
                            checkLock gameState 0
                            atomically $ do
                                            newState <- readTVar gameState 
                                            writeTVar gameState (newState {lastKeyStroke = (Char '_')})
                            return gameState
                _          -> return gameState
        else return gameState


main = do
    if lineOfSight < 0 || maxWaterLevel < 0 || waterRefill < 0 || treasureProb < 0 || waterProb < 0 || portalProb < 0 || lavaProb < 0 || adjLavaProb < 0 || maxPathLength < 0 || renderWidth <= 0 || renderHeight <= 0 || maxWormLength <= 0 || wormProb < 0
        then do
            print "ERROR : negative parameters detected ! please provide only positive values for each parameter ! (renderWidth and renderHeight must be greater than 0 as well)"
            exitWith ExitSuccess
        else return ()
    if (waterProb + portalProb + lavaProb > 100) || (waterProb + portalProb + adjLavaProb > 100)
        then do
            print "ERROR : spawning probabilities sum is greater than 100 !"
            exitWith ExitSuccess
        else return ()
    let initialGameMap = (randGen `seq` generateMatrix)
    initialState <- initialGameMap `seq` newTVarIO (State {xPos = 0
                                                    , yPos = 0
                                                    , waterLevel = maxWaterLevel
                                                    , tilesLooted = []
                                                    , tilesVisible = rmdups (detectTiles 0 0 [])
                                                    , gameMap = initialGameMap
                                                    , score = 0
                                                    , wormPaths = []
                                                    , gameMode = 0
                                                    , stepsTaken = 0
                                                    , availableThreads = []
                                                    , threadCount = 0
                                                    , gameStatus = 0 
                                                    , lastKeyStroke = (Char '_')})
    initialState `seq` createForkedThreads initialState 1
    playIO (InWindow "Treasure Hunter" (1024, 600) (10, 10))
                            black
                            20
                            initialState
                            renderWindow
                            getInput
                            processInput