import System.IO
import System.Exit
import System.Directory
import System.Random
import Data.List
import Data.List.Split
import Control.DeepSeq
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Control.Monad
import Control.Parallel
import Control.Concurrent
import Control.Concurrent.STM

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

saveFileName = "game.sav"

-- Misc function for removing duplicates
-- taken from StackOverflow
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

-- Tile generation parameters

data GameParam = GameParam {  treasureProb :: Int
                            , waterProb :: Int
                            , portalProb :: Int
                            , lavaProb :: Int
                            , adjLavaProb :: Int
                            , wormProb :: Int
                            , maxWormLength :: Int
                            , lineOfSight :: Int 
                            , maxWaterLevel :: Int
                            , waterRefill :: Int
                            , randGenSeed :: Int}

-- Player/State related functions

data State = State { playerPos :: [Int]
                    , waterLevel :: Int
                    , tilesLooted :: [[Int]]
                    , tilesVisible :: [[Int]]
                    , gameMap :: [Tile]
                    , score :: Int
                    , wormPaths :: [[[Int]]]
                    , gameMode :: Int
                    , nextRand :: Int
                    , availableThreads :: [Int]
                    , threadCount :: Int
                    , gameStatus :: Int
                    , lastKeyStroke :: Key
                    , param :: GameParam }

-- Moves player one tile in the specified direction and reduces water by one unit
movePlayerUp :: TVar State -> IO ()
movePlayerUp gameState = do
    state <- readTVarIO gameState
    if (last (playerPos state)) > 0
        then do
            newState <- movePlayer (state { playerPos = [(head (playerPos state)),(last (playerPos state)) - 1], waterLevel = (waterLevel state) - 1 })
            atomically $ writeTVar gameState (newState {nextRand = (length (tilesVisible newState))})
            generateWorm gameState
            return ()
        else return ()

movePlayerDown :: TVar State -> IO ()
movePlayerDown gameState = do
    state <- readTVarIO gameState
    newState <- movePlayer (state { playerPos = [(head (playerPos state)),(last (playerPos state)) + 1], waterLevel = (waterLevel state) - 1 })
    atomically $ writeTVar gameState (newState {nextRand = (length (tilesVisible newState))})
    generateWorm gameState
    return ()

movePlayerLeft :: TVar State -> IO ()
movePlayerLeft gameState = do
    state <- readTVarIO gameState
    if (head (playerPos state)) > 0
        then do
            newState <- movePlayer (state { playerPos = [(head (playerPos state)) - 1,(last (playerPos state))], waterLevel = (waterLevel state) - 1 })
            atomically $ writeTVar gameState (newState {nextRand = (length (tilesVisible newState))})
            generateWorm gameState
            return ()
        else return ()

movePlayerRight :: TVar State -> IO ()
movePlayerRight gameState = do
    state <- readTVarIO gameState
    newState <- movePlayer (state { playerPos = [(head (playerPos state)) + 1,(last (playerPos state))], waterLevel = (waterLevel state) - 1 })
    atomically $ writeTVar gameState (newState {nextRand = (length (tilesVisible newState))})
    generateWorm gameState
    return ()

-- Refills player's water by a constant amount of units
refillWater :: State -> IO State
refillWater state = 
    return (state { waterLevel = minimum([(maxWaterLevel (param state)),(waterLevel state)+(waterRefill (param state))]) })

-- Handles flow of the game as the player moves
movePlayer :: State -> IO State
movePlayer state =
    if (waterLevel state) < 0
        then do
            return (state {gameMode = -1})
        else do
            let newTilesVisible = (tilesVisible state) ++ (detectTiles currX currY (tilesVisible state) (lineOfSight (param state)))
                tilesVisible' = newTilesVisible `deepseq` rmdups newTilesVisible
                currX = head (playerPos state)
                currY = last (playerPos state)
                currTile = currX `seq` currY `seq` getTile currX currY (gameMap state)
                currTileType = currTile `seq` tileType currTile
            case currTileType of 
                'd' ->  do
                    if elem [currX,currY] (filter (\x -> if (head x) >= 0 && (last x) >= 0 then True else False) (concat (wormPaths state)))
                        then return (state {gameMode = -4})
                        else if (elem [currX,currY] (tilesLooted state)) || not (hasTreasure currTile)
                            then return (state { tilesVisible = tilesVisible' })
                            else return (state { tilesLooted = ((tilesLooted state) ++ [[currX,currY]]), tilesVisible = tilesVisible', score = (score state) + 1 })
                'l' ->  do
                    return (state {gameMode = -2})
                'p' ->  do
                    return (state {gameMode = -3})
                'w' ->  do
                    if elem [currX,currY] (tilesLooted state)
                        then return (state {tilesVisible = tilesVisible'})
                        else refillWater (state { tilesLooted = ((tilesLooted state) ++ [[currX,currY]]), tilesVisible = tilesVisible' })


-- gameMap related functions

data Tile = Tile { tileType :: Char, hasTreasure :: Bool }
    deriving Show  

-- Randomly generates a tile type
randomTile :: Int -> Int -> Int -> GameParam -> Tile
randomTile x y num gameParam = do
    let randNum = (randomRs (1,100) (mkStdGen (randGenSeed gameParam)) :: [Int]) !! num
    if randNum `seq` (randNum <= (waterProb gameParam))
        then Tile { tileType = 'w', hasTreasure = False }
        else if ((waterProb gameParam) + (portalProb gameParam) ) `seq` (randNum <= ((waterProb gameParam) + (portalProb gameParam) ))
            then Tile { tileType = 'p', hasTreasure = False }
            else if ((waterProb gameParam) + (portalProb gameParam) + (lavaProb gameParam)) `seq` (randNum <= (waterProb gameParam) + (portalProb gameParam) + (lavaProb gameParam))
                then Tile { tileType = 'l', hasTreasure = False }
                else do
                    let randNum = (num+1) `seq` (randomRs (1,100) (mkStdGen (randGenSeed gameParam)) :: [Int]) !! (num+1)
                    if randNum `seq` (randNum <= (treasureProb gameParam))
                        then Tile { tileType = 'd', hasTreasure = True }
                        else Tile { tileType = 'd', hasTreasure = False }

-- Randomly generates a tile type (adjacent to a lava tile)
randomTile' :: Int -> Int -> Int -> GameParam -> Tile
randomTile' x y num gameParam = do
    let randNum = (randomRs (1,100) (mkStdGen (randGenSeed gameParam)) :: [Int]) !! num
    if randNum `seq` (randNum <= (waterProb gameParam))
        then Tile { tileType = 'w', hasTreasure = False }
        else if ((waterProb gameParam) + (portalProb gameParam) ) `seq` (randNum <= ((waterProb gameParam) + (portalProb gameParam) ))
            then Tile { tileType = 'p', hasTreasure = False }
            else if ((waterProb gameParam) + (portalProb gameParam) + (adjLavaProb gameParam)) `seq` (randNum <= (waterProb gameParam) + (portalProb gameParam) + (adjLavaProb gameParam))
                then Tile { tileType = 'l', hasTreasure = False }
                else do
                    let randNum = (num+1) `seq` (randomRs (1,100) (mkStdGen (randGenSeed gameParam)) :: [Int]) !! (num+1)
                    if randNum `seq` (randNum <= (treasureProb gameParam))
                        then Tile { tileType = 'd', hasTreasure = True }
                        else Tile { tileType = 'd', hasTreasure = False }

-- Randomly generates a complete tile
generateTile :: Int -> Int -> [Tile] -> Int -> GameParam -> Tile
generateTile x y gameMap num gameParam 
    | x > 0 && y > 0 = do
        let adjYTile = (y-1) `seq` getTile x (y-1) gameMap
            adjXTile = (x-1) `seq` getTile (x-1) y gameMap
        if adjXTile `seq` adjYTile `seq` ((tileType adjXTile) == 'l' || (tileType adjYTile) == 'l')
            then randomTile' x y num gameParam
            else randomTile x y num gameParam
    | x > 0 = do
        let adjXTile = (x-1) `seq` getTile (x-1) y gameMap
        if adjXTile `seq` ((tileType adjXTile) == 'l')
            then randomTile' x y num gameParam
            else randomTile x y num gameParam
    | y > 0 = do
        let adjYTile = (y-1) `seq` getTile x (y-1) gameMap
        if adjYTile `seq` ((tileType adjYTile) == 'l')
            then randomTile' x y num gameParam
            else randomTile x y num gameParam
    | otherwise = Tile { tileType = 'd', hasTreasure = False }

-- Generates an infinite gameMap in the form of a matrix (it is however kept in memory as a list)
-- Starter function 
generateMatrix :: GameParam -> [Tile]
generateMatrix gameParam = generateMatrix' 0 0 [] 1 gameParam

generateMatrix' :: Int -> Int -> [Tile] -> Int -> GameParam -> [Tile]
generateMatrix' 0 0 gameMap num gameParam = do
    let currTile = generateTile 0 0 gameMap num gameParam
    currTile `seq` ([currTile] ++ ((gameMap ++ [currTile]) `seq` (num+2) `seq` (generateMatrix' 1 0 (gameMap ++ [currTile]) (num+2) gameParam)))
generateMatrix' x 0 gameMap num gameParam = do
    let currTile = generateTile x 0 gameMap num gameParam
    currTile `seq` ([currTile] ++ ((gameMap ++ [currTile]) `seq` (num+2) `seq` (generateMatrix' 0 x (gameMap ++ [currTile]) (num+2) gameParam)))
generateMatrix' 0 y gameMap num gameParam = do
    let currTile = generateTile 0 y gameMap num gameParam
    currTile `seq` ([currTile] ++ ((gameMap ++ [currTile]) `seq` (num+2) `seq` (generateMatrix' y 1 (gameMap ++ [currTile]) (num+2) gameParam)))
generateMatrix' x y gameMap num gameParam
    | x > y = do
        let currTile = generateTile x y gameMap num gameParam
        currTile `seq` ([currTile] ++ ((gameMap ++ [currTile]) `seq` (num+2) `seq` (generateMatrix' y x (gameMap ++ [currTile]) (num+2) gameParam)))
    | x < y = do
        let currTile = generateTile x y gameMap num gameParam
        currTile `seq` ([currTile] ++ ((gameMap ++ [currTile]) `seq` (num+2) `seq` (x+1) `seq` (generateMatrix' y (x+1) (gameMap ++ [currTile]) (num+2) gameParam)))
    | x == y = do
        let currTile = generateTile x y gameMap num gameParam
        currTile `seq` ([currTile] ++ ((gameMap ++ [currTile]) `seq` (num+2) `seq` (x+1) `seq` (generateMatrix' (x+1) 0 (gameMap ++ [currTile]) (num+2) gameParam)))

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
detectTiles :: Int -> Int -> [[Int]] -> Int -> [[Int]]
detectTiles x y tilesVisible los = detectTiles' x y 0 0 tilesVisible los

detectTiles' :: Int -> Int -> Int -> Int -> [[Int]] -> Int -> [[Int]]
detectTiles' x y nx ny tilesVisible los
    | ny == (los*2) = do
        if not (elem [x,(y+los)] tilesVisible)
            then (y+los) `seq` [[x,(y+los)]]
            else []
    | ny >= 0 && ny < los = do
        if (nx <= ny)
            then ((x+nx) `seq` (y-(los-ny)) `deepseq` [[(x+nx),(y-(los-ny))]]) ++ ((nx+1) `seq` detectTiles' x y (nx+1) ny tilesVisible los)
            else [] ++ ((ny+1) `seq` (negate (ny+1)) `seq` detectTiles' x y (negate (ny+1)) (ny+1) tilesVisible los)
    | ny >= los && ny < (los*2) = do
        if (nx <= (los-(mod ny los)))
            then ((x+nx) `seq` (y+(ny-los)) `deepseq` [[(x+nx),(y+(ny-los))]]) ++ ((nx+1) `deepseq` detectTiles' x y (nx+1) ny tilesVisible los)
            else [] ++ ((ny+1) `seq` (negate (los-(mod (ny+1) los))) `deepseq` detectTiles' x y (negate (los-(mod (ny+1) los))) (ny+1) tilesVisible los)

-- Searches for a specific tile type using Depth-First-Search and returns the length of the shortest path found
-- Starter function
getClosestTile :: Int -> Int -> Char -> [[Int]] -> [Tile] -> GameParam -> String
getClosestTile sx sy targetTile tilesLooted gameMap gameParam = do 
    if (targetTile == 'w' && (waterProb gameParam) > 0)
        then do
            let waterDist = getClosestTile'' sx sy targetTile 0 tilesLooted [] gameMap
            if waterDist == maxPathLength*2
                then ((show maxPathLength)++"+")
                else (show waterDist)
        else if (targetTile == 'p' && (portalProb gameParam) > 0)
            then do
                let portalDist = getClosestTile'' sx sy targetTile 0 tilesLooted [] gameMap
                if portalDist == maxPathLength*2
                    then ((show maxPathLength)++"+")
                    else (show portalDist)
            else if (targetTile == 'd' && (treasureProb gameParam) > 0) 
                then do
                    let treasureDist = getClosestTile'' sx sy targetTile 0 tilesLooted [] gameMap
                    if treasureDist == maxPathLength*2
                        then ((show maxPathLength)++"+")
                        else (show treasureDist)
                else "None"

-- ... using lazy evaluation
getClosestTile' :: Int -> Int -> Char -> Int -> [[Int]] -> [[Int]] -> [Tile] -> Int
getClosestTile' sx sy targetTile steps tilesLooted pathTaken gameMap
    | (sx < 0) || (sy < 0) || (steps > maxPathLength) || (elem [sx,sy] pathTaken) = (maxPathLength*2)
    | otherwise = do
        let currTile = getTile sx sy gameMap
        if (targetTile == (tileType currTile) && (not (elem [sx,sy] tilesLooted))) && (((targetTile == 'd') && (hasTreasure currTile)) || (targetTile /= 'd'))
            then steps
            else do
                if (tileType currTile) /= 'l' && (tileType currTile) /= 'p'
                    then do
                        let allPaths = [(getClosestTile' (sx+1) sy targetTile (steps+1) tilesLooted (pathTaken++[[sx,sy]]) gameMap)] 
                            allPaths' = allPaths ++ [(getClosestTile' sx (sy+1) targetTile (steps+1) tilesLooted (pathTaken++[[sx,sy]]) gameMap)] 
                            allPaths'' = allPaths' ++ [(getClosestTile' (sx-1) sy targetTile (steps+1) tilesLooted (pathTaken++[[sx,sy]]) gameMap)] 
                            allPaths''' = allPaths'' ++ [(getClosestTile' sx (sy-1) targetTile (steps+1) tilesLooted (pathTaken++[[sx,sy]]) gameMap)]
                        minimum (allPaths''')
                    else (maxPathLength*2)

-- using strict evaluation
getClosestTile'' :: Int -> Int -> Char -> Int -> [[Int]] -> [[Int]] -> [Tile] -> Int
getClosestTile'' sx sy targetTile steps tilesLooted pathTaken gameMap
    | (sx < 0) || (sy < 0) || (steps > maxPathLength) || (elem [sx,sy] pathTaken) = (maxPathLength*2)
    | otherwise = do
        let currTile = gameMap `seq` getTile sx sy gameMap
        if currTile `seq` ((targetTile == (tileType currTile) && (not (elem [sx,sy] tilesLooted))) && (((targetTile == 'd') && (hasTreasure currTile)) || (targetTile /= 'd')))
            then steps
            else do
                if (tileType currTile) `seq` ((tileType currTile) /= 'l' && (tileType currTile) /= 'p')
                    then do
                        let allPaths =  ((sx+1) `seq` (steps+1) `seq` (pathTaken++[[sx,sy]]) `seq` [(getClosestTile'' (sx+1) sy targetTile (steps+1) tilesLooted (pathTaken++[[sx,sy]]) gameMap)]) 
                            allPaths' = allPaths `deepseq` (allPaths ++ ((sy+1) `seq` (steps+1) `seq` (pathTaken++[[sx,sy]]) `seq` [(getClosestTile'' sx (sy+1) targetTile (steps+1) tilesLooted (pathTaken++[[sx,sy]]) gameMap)]))
                            allPaths'' = allPaths' `deepseq` (allPaths' ++ ((sx-1) `seq` (steps+1) `seq` (pathTaken++[[sx,sy]]) `seq` [(getClosestTile'' (sx-1) sy targetTile (steps+1) tilesLooted (pathTaken++[[sx,sy]]) gameMap)]) )
                            allPaths''' = allPaths'' `deepseq` (allPaths'' ++ ((sy-1) `seq` (steps+1) `seq` (pathTaken++[[sx,sy]]) `seq` [(getClosestTile'' sx (sy-1) targetTile (steps+1) tilesLooted (pathTaken++[[sx,sy]]) gameMap)]))
                        allPaths''' `deepseq` minimum (allPaths''')
                    else (maxPathLength*2)

generateWorm :: TVar State -> IO ()
generateWorm gameState = do
    state <- readTVarIO gameState
    let randNum = (randomRs (1,100) (mkStdGen (randGenSeed (param state))) :: [Int]) !! (nextRand state)
    if randNum `seq` (randNum <= (wormProb (param state))) 
        then do
            if (length (availableThreads state)) > 0
                then do 
                    let newCoordPool = filter (\x -> getValidWormTiles [-2,-2] x (playerPos state) (wormPaths state) [] (tilesLooted state) (gameMap state)) (detectTiles (head (playerPos state)) (last (playerPos state)) (tilesVisible state) (lineOfSight (param state)))
                    if ((length newCoordPool) > 0)
                        then do
                            let chosenCoords = (newCoordPool!!(randNum `mod` (length newCoordPool)))
                                chosenThread = head (availableThreads state)
                            atomically $ writeTVar gameState (state {
                                                            wormPaths = (take (chosenThread) (wormPaths state)) ++ [[[-1,-1], chosenCoords]] ++ (drop (chosenThread+1) (wormPaths state))
                                                            , availableThreads = tail (availableThreads state)
                                                            , gameStatus = ((threadCount state)-(length (availableThreads state))+1) })
                            return ()
                        else atomically $ writeTVar gameState (state {gameStatus = (threadCount state)})
                else do
                    (createForkedThreads gameState False 1) >> (generateWorm gameState)
        else atomically $ writeTVar gameState (state {gameStatus = ((threadCount state)-(length (availableThreads state)))}) 

controlWorm :: TVar State -> Int -> IO ()
controlWorm gameState threadIndex = do
    state <- readTVarIO gameState
    if (gameStatus state) > 0 
        && ((head ((wormPaths state)!!threadIndex)) /= [-1,-1] || ((head ((wormPaths state)!!threadIndex)) == [-1,-1] && (length ((wormPaths state)!!threadIndex)) > 1))
        then do
            if (head ((wormPaths state)!!threadIndex)) /= [-1,-1]
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
    if (last currWormPath) /= [-1,-1]
        then do
            let newCoordPool = filter (\x -> 
                                    getValidWormTiles (head currWormPath) x (playerPos state) (wormPaths state) currWormPath (tilesLooted state) (gameMap state))
                                    [[(head (head currWormPath))+1,(last (head currWormPath))]
                                    ,[(head (head currWormPath))-1,(last (head currWormPath))]
                                    ,[(head (head currWormPath)),(last (head currWormPath))+1]
                                    ,[(head (head currWormPath)),(last (head currWormPath))-1]]
            if (length newCoordPool) > 0 && (length currWormPath) < (maxWormLength (param state))
                then do
                    let newCoord = head (sortBy (\a b -> compare (getDistance a (playerPos state)) (getDistance b (playerPos state))) newCoordPool)
                    atomically $ do 
                        newState <- readTVar gameState
                        writeTVar gameState (newState {
                            wormPaths = (take (threadIndex) (wormPaths newState)) ++ [[newCoord] ++ ((wormPaths newState)!!threadIndex)] ++ (drop (threadIndex+1) (wormPaths newState))
                            , gameStatus = (gameStatus newState)-1 })
                else if (length currWormPath) > 1
                    then atomically $ do 
                            newState <- readTVar gameState
                            writeTVar gameState (newState {
                                wormPaths = (take (threadIndex) (wormPaths newState)) ++ [(tail (reverse ((wormPaths newState)!!threadIndex))) ++ [[-1,-1]]] ++ (drop (threadIndex+1) (wormPaths newState))
                                , gameStatus = (gameStatus newState)-1 })
                    else atomically $ do 
                            newState <- readTVar gameState
                            writeTVar gameState (newState {
                                wormPaths = (take (threadIndex) (wormPaths newState)) ++ [[[-1,-1]]] ++ (drop (threadIndex+1) (wormPaths newState))
                                , availableThreads = (availableThreads newState) ++ [threadIndex]
                                , gameStatus = (gameStatus newState)-1 })
        else if (head (tail ((wormPaths state)!!threadIndex))) /= [-1,-1]
            then atomically $ do 
                    newState <- readTVar gameState
                    writeTVar gameState (newState {
                        wormPaths = (take (threadIndex) (wormPaths newState)) ++ [tail ((wormPaths newState)!!threadIndex)] ++ (drop (threadIndex+1) (wormPaths newState))
                        , gameStatus = (gameStatus newState)-1 })
            else atomically $ do 
                    newState <- readTVar gameState
                    writeTVar gameState (newState {
                        wormPaths = (take (threadIndex) (wormPaths newState)) ++ [[[-1,-1]]] ++ (drop (threadIndex+1) (wormPaths newState))
                        , availableThreads = (availableThreads newState) ++ [threadIndex]
                        , gameStatus = (gameStatus newState)-1 })


getDistance :: [Int] -> [Int] -> Float
getDistance startCoords endCoords = abs(sqrt(fromIntegral ((((head endCoords) - (head startCoords))^2)+(((last endCoords) - (last startCoords))^2))))

getValidWormTiles :: [Int] -> [Int] -> [Int] -> [[[Int]]] -> [[Int]] -> [[Int]] -> [Tile] -> Bool
getValidWormTiles headCoords coords playerCoords allWormPaths currWormPath tilesLooted gameMap = do
    if (not (elem coords ((getProjectedWormBodyTiles headCoords allWormPaths) ++ (concat allWormPaths))))
        && (not ((head coords) < 0 || (last coords) < 0))
        && (not ((head playerCoords) == (head coords) && (last playerCoords) == (last coords)))
        then do
            let currTile = getTile (head coords) (last coords) gameMap
                currTileType = currTile `seq` tileType currTile
            if (currTileType == 'd' && (not (hasTreasure currTile) || elem coords tilesLooted))
                then True
                else False
        else False

getProjectedWormBodyTiles :: [Int] -> [[[Int]]] -> [[Int]]
getProjectedWormBodyTiles coords allWormPaths
    | (length allWormPaths) > 0 = do
        let currWormPath = head allWormPaths
        if (head currWormPath) /= coords && (head currWormPath) /= [-1,-1]
            then [[(head (head currWormPath))+1,(last (head currWormPath))]
                 ,[(head (head currWormPath))-1,(last (head currWormPath))]
                 ,[(head (head currWormPath)),(last (head currWormPath))+1]
                 ,[(head (head currWormPath)),(last (head currWormPath))-1]] ++ (getProjectedWormBodyTiles coords (tail allWormPaths))
            else (getProjectedWormBodyTiles coords (tail allWormPaths))
    | otherwise = []

-- Prints out the map and HUD
-- Starter function
renderWindow :: TVar State -> IO Picture
renderWindow gameState = do
    checkLock gameState 0
    state <- readTVarIO gameState
    return ((negate renderWidth) `deepseq` (negate renderHeight) `deepseq` pictures ((renderState state) ++ (renderGameMap (head (playerPos state)) (last (playerPos state)) (negate renderWidth) (negate renderHeight) (tilesVisible state) (gameMap state)) ++ (renderPlayer) ++ (renderWorms (head (playerPos state)) (last (playerPos state)) (negate renderWidth) (negate renderHeight) (tilesVisible state) (wormPaths state)) ++ (renderGameOver state)))

-- Prints out the "status" section of the display with information such as water left, treasures found
-- and distance to nearby points of interest
renderState :: State -> [Picture]
renderState state = 
    [translate (250+xOffset) (263.5+yOffset) $ color (greyN 0.75) $ rectangleSolid 695 54 
    , translate (214+xOffset) (263.5+yOffset) $ rectangleWire 695 54
    , translate (-112.5+xOffset) (263.5+yOffset) $ color (greyN 0.75) $ rectangleSolid 695 54
    , translate (400+xOffset) (243+yOffset) $ scale 0.15 0.15 $ Text ("K : Suicide")
    , translate (406+xOffset) (251+yOffset) $ rectangleWire 21 21
    , translate (400+xOffset) (270+yOffset) $ scale 0.15 0.15 $ Text ("L : Save Game")
    , translate (406+xOffset) (278+yOffset) $ rectangleWire 21 21
    , translate (271+xOffset) (270+yOffset) $ scale 0.15 0.15 $ Text ("W")
    , translate (278.5+xOffset) (278+yOffset) $ rectangleWire 21 21
    , translate (245+xOffset) (243+yOffset) $ scale 0.15 0.15 $ Text ("A S D")
    , translate (251+xOffset) (251+yOffset) $ rectangleWire 21 21
    , translate (278.5+xOffset) (251+yOffset) $ rectangleWire 21 21
    , translate (306+xOffset) (251+yOffset) $ rectangleWire 21 21
    , translate (321+xOffset) (257+yOffset) $ scale 0.15 0.15 $ Text (": Move")
    , translate (-450+xOffset) (243+yOffset) $ scale 0.15 0.15 $ Text ("Nearest water : " ++ getClosestTile (head (playerPos state)) (last (playerPos state)) 'w' (tilesLooted state) (gameMap state) (param state))
    , translate (-225+xOffset) (243+yOffset) $ scale 0.15 0.15 $ Text ("Nearest portal : " ++ getClosestTile (head (playerPos state)) (last (playerPos state)) 'p' (tilesLooted state) (gameMap state) (param state))
    , translate (0+xOffset) (243+yOffset) $ scale 0.15 0.15 $ Text ("Nearest treasure : " ++ getClosestTile (head (playerPos state)) (last (playerPos state)) 'd' (tilesLooted state) (gameMap state) (param state))
    , translate (-400+xOffset) (270+yOffset) $ scale 0.15 0.15 $ Text ("Water units left : " ++ (if ((waterLevel state) == 0 || (waterLevel state) == (-1)) then "Dry" else show (waterLevel state)))
    , translate (-70+xOffset) (270+yOffset) $ scale 0.15 0.15 $ Text ("Treasures found  : " ++ (show (score state)))
    , translate (-345+xOffset) (250+yOffset) $ rectangleWire 230 27
    , translate (-120+xOffset) (250+yOffset) $ rectangleWire 220 27
    , translate (112.5+xOffset) (250+yOffset) $ rectangleWire 245 27
    , translate (-286.5+xOffset) (277+yOffset) $ rectangleWire 347 27
    , translate (61+xOffset) (277+yOffset) $ rectangleWire 348 27
    ]

renderGameMap :: Int -> Int -> Int -> Int -> [[Int]] -> [Tile] -> [Picture]
renderGameMap x y nx ny tilesVisible gameMap
    | ny > renderHeight = []
    | nx <= renderWidth = do
        if (x+nx) `seq` (y+ny) `seq` elem [(x+nx),(y+ny)] tilesVisible
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

renderPlayer :: [Picture]
renderPlayer = [color green $ circleSolid 9
                        , circle 9
                        , translate (-4) (-5) $ scale 0.1 0.1 $ Text "P"
                        ]

renderWorms :: Int -> Int -> Int -> Int -> [[Int]] -> [[[Int]]] -> [Picture]
renderWorms x y nx ny tilesVisible allWormPaths
    | ny > renderHeight = []
    | nx <= renderWidth = do
        if (x+nx) `seq` (y+ny) `seq` elem [(x+nx),(y+ny)] tilesVisible
            then do
                if elem [(x+nx),(y+ny)] (filter (\x -> if (head x) >= 0 && (last x) >= 0 then True else False) (concat allWormPaths))
                    then if (isWormHead [(x+nx),(y+ny)] allWormPaths)
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

isWormHead :: [Int] -> [[[Int]]] -> Bool
isWormHead coords allWormPaths
    | (length allWormPaths) > 0 = if ((head (head allWormPaths)) == coords && (last (head allWormPaths)) /= [-1,-1]) 
                                  || ((head (head allWormPaths)) == [-1,-1] && (last (head allWormPaths)) == coords)
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
    | (gameMode state == -6) = [color (greyN 0.44) $ rectangleSolid 900 500
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

createForkedThreads :: TVar State -> Bool -> Int -> IO ()
createForkedThreads gameState justFork numThreads
    | (numThreads > 0) = do
        state <- readTVarIO gameState
        forkIO $ controlWorm gameState (threadCount state)
        if (not justFork)
            then
                atomically $ writeTVar gameState (state {wormPaths = ((wormPaths state) ++ [[[-1,-1]]])
                                                        , availableThreads = (availableThreads state) ++ [(threadCount state)]
                                                        , threadCount = (threadCount state)+1})
            else
                atomically $ writeTVar gameState (state {threadCount = (threadCount state)+1})
        createForkedThreads gameState justFork (numThreads-1)
    | otherwise = return ()

resetWormThreads :: [[[Int]]] -> [[[Int]]]
resetWormThreads allWormPaths
    | (length allWormPaths) > 0 = [[[-1,-1]]] ++ resetWormThreads (tail allWormPaths)
    | otherwise = []

getInput :: Event -> TVar State -> IO (TVar State)
getInput (EventKey key keyState _ _) gameState = do
    state <- readTVarIO gameState
    if (keyState == Down)
        then if ((gameMode state) == 0)
            then
                case key of
                    (SpecialKey KeyEsc) -> exitWith ExitSuccess
                    (Char 'l') -> do 
                                state <- readTVarIO gameState
                                print "Saving game..." 
                                state `seq` saveGame state
                                print "Game saved ! (game.sav)"
                                return gameState
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
                then 
                    case key of
                        (SpecialKey KeyEsc) -> exitWith ExitSuccess
                        _   -> do
                                atomically $ writeTVar gameState (state {playerPos = [0,0]
                                                                        , waterLevel = (maxWaterLevel (param state))
                                                                        , tilesLooted = []
                                                                        , tilesVisible = rmdups (detectTiles 0 0 [] (lineOfSight (param state)))
                                                                        , score = 0
                                                                        , wormPaths = resetWormThreads (wormPaths state)
                                                                        , gameMode = 0
                                                                        , nextRand = 0
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


-- Parser
deleteAllOccurences :: [Char] -> String -> String
deleteAllOccurences charsToRemove origString
    | (length origString > 0) && (elem (head origString) charsToRemove) = deleteAllOccurences charsToRemove (tail origString)
    | (length origString > 0) = [(head origString)] ++ deleteAllOccurences charsToRemove (tail origString)
    | otherwise = []

parseNumber :: String -> Int
parseNumber origString = read origString::Int

parseCoordinates :: String -> [[Int]]
parseCoordinates origString 
    | (length origString > 0) && ('[' == (head origString)) && (elem ']' origString) = do
        let index = (\(Just i)->i) (elemIndex ']' origString)
            coordString = splitOn "," (deleteAllOccurences "[]" (take (index+1) origString))
        [[read (head coordString)::Int, read (last coordString)::Int]] ++ parseCoordinates (drop (index+2) origString)
    | otherwise = []

parseLine :: String -> [String]
parseLine origString =
    splitOn "(" (deleteAllOccurences " )" origString)

parseAllLines :: [String] -> [[String]]
parseAllLines fileContents
    | (length fileContents) > 0 = [parseLine (head fileContents)] ++ parseAllLines (tail fileContents)
    | otherwise = []

getGameState :: [[String]] -> State -> State
getGameState fileContents state
    | (length fileContents) > 0 = 
        case (head (head fileContents)) of
            "position"      -> getGameState (tail fileContents) (state {playerPos = (head (parseCoordinates (last (head fileContents))))})
            "supply"        -> getGameState (tail fileContents) (state {waterLevel = (parseNumber (last (head fileContents)))})
            "revealed"      -> getGameState (tail fileContents) (state {tilesVisible = (tilesVisible state) ++ (parseCoordinates (last (head fileContents)))})
            "collected"     -> getGameState (tail fileContents) (state {tilesLooted = (tilesLooted state) ++ (parseCoordinates (last (head fileContents)))})
            "emerging"      -> getGameState (tail fileContents) (state {wormPaths = (wormPaths state) ++ [parseCoordinates (last (head fileContents))]})
            "disappearing"  -> getGameState (tail fileContents) (state {wormPaths = (wormPaths state) ++ [parseCoordinates (last (head fileContents)) ++ [[-1,-1]]]})
            "s"             -> getGameState (tail fileContents) (state {param = ((param state) {lineOfSight = (parseNumber (last (head fileContents)))})})
            "m"             -> getGameState (tail fileContents) (state {param = ((param state) {maxWaterLevel = (parseNumber (last (head fileContents)))})})
            "g"             -> getGameState (tail fileContents) (state {param = ((param state) {randGenSeed = (parseNumber (last (head fileContents)))})})
            "t"             -> getGameState (tail fileContents) (state {param = ((param state) {treasureProb = (parseNumber (last (head fileContents)))})})
            "w"             -> getGameState (tail fileContents) (state {param = ((param state) {waterProb = (parseNumber (last (head fileContents)))})})
            "p"             -> getGameState (tail fileContents) (state {param = ((param state) {portalProb = (parseNumber (last (head fileContents)))})})
            "l"             -> getGameState (tail fileContents) (state {param = ((param state) {lavaProb = (parseNumber (last (head fileContents)))})})
            "ll"            -> getGameState (tail fileContents) (state {param = ((param state) {adjLavaProb = (parseNumber (last (head fileContents)))})})
            "x"             -> getGameState (tail fileContents) (state {param = ((param state) {maxWormLength = (parseNumber (last (head fileContents)))})})
            "y"             -> getGameState (tail fileContents) (state {param = ((param state) {wormProb = (parseNumber (last (head fileContents)))})})
            _               -> getGameState (tail fileContents) state
    | otherwise = state {score = (length (tilesLooted state)), nextRand = (length (tilesVisible state))}

parseSaveFile :: State -> String -> IO State
parseSaveFile state filePath = do
    rawFileContents <- (readFile filePath)
    let fileContents =  parseAllLines (splitOn "\n" rawFileContents)
    return (getGameState fileContents state)

getVisibleTiles :: [[Int]] -> String
getVisibleTiles tiles
    | (length tiles) > 0 = "revealed(" ++ (show $ head tiles) ++ ")\n" ++ (getVisibleTiles $ tail tiles)
    | otherwise = ""

getLootedTiles :: [[Int]] -> String
getLootedTiles tiles
    | (length tiles) > 0 = "collected(" ++ (show $ head tiles) ++ ")\n" ++ (getLootedTiles $ tail tiles)
    | otherwise = ""

getWorms :: [[[Int]]] -> String
getWorms allWormPaths
    | (length allWormPaths) > 0 && ((<) 1 . length $ head allWormPaths) && (last $ head allWormPaths) == [-1,-1] = "disappearing(" ++ (show $ init $ head allWormPaths) ++ ")\n" ++ (getWorms $ tail allWormPaths)
    | (length allWormPaths) > 0 && (last $ head allWormPaths) /= [-1,-1] = "emerging(" ++ (show $ head allWormPaths) ++ ")\n" ++ (getWorms $ tail allWormPaths)
    | (length allWormPaths) > 0 = getWorms $ tail allWormPaths
    | otherwise = ""

saveGame :: State -> IO ()
saveGame state = do
    fileExists <- doesFileExist "game.sav"
    when fileExists $ removeFile "game.sav"
    let outString = "position(" ++ (show (playerPos state)) ++ ")\n" ++
                    "supply(" ++ (show (waterLevel state)) ++ ")\n" ++
                    (getVisibleTiles $ tilesVisible state) ++
                    (getLootedTiles $ tilesLooted state) ++
                    (getWorms $ wormPaths state) ++
                    "s(" ++ (show $ lineOfSight $ param state) ++ ")\n" ++
                    "m(" ++ (show $ maxWaterLevel $ param state) ++ ")\n" ++
                    "g(" ++ (show $ randGenSeed $ param state) ++ ")\n" ++                    
                    "t(" ++ (show $ treasureProb $ param state) ++ ")\n" ++
                    "w(" ++ (show $ waterProb $ param state) ++ ")\n" ++                    
                    "p(" ++ (show $ portalProb $ param state) ++ ")\n" ++
                    "l(" ++ (show $ lavaProb $ param state) ++ ")\n" ++                    
                    "ll(" ++ (show $ adjLavaProb $ param state) ++ ")\n" ++
                    "x(" ++ (show $ maxWormLength $ param state) ++ ")\n" ++                    
                    "y(" ++ (show $ wormProb $ param state) ++ ")\n"
    writeFile "game.sav" outString
    return ()

gameModeQuery :: State -> IO State
gameModeQuery state = do
    print "Choose an option by typing the corresponding number followed by ENTER :"
    print "(1) New game"
    print "(2) Load game"
    input <- getValidInput ["1","2"]
    case input of
        2   -> do
            print "Please type the name of the save file to load (with file extension) followed by ENTER :" 
            getValidSaveFile state
        _   -> return state

getValidInput :: [String] -> IO Int
getValidInput choices = do
    input <- getLine
    if (elem input choices)
        then
            return (read input::Int)
        else do
            print "ERROR : Invalid input"
            getValidInput choices

getValidSaveFile :: State -> IO State
getValidSaveFile state = do
    input <- getLine
    fileExists <- doesFileExist input
    if (fileExists)
        then
            parseSaveFile state input
        else do
            print "ERROR : File not found ! Returning to main menu ..."
            gameModeQuery state

-- Main
main = do
    let defaultParam = GameParam {  treasureProb = 33
                                , waterProb = 10
                                , portalProb = 5
                                , lavaProb = 15
                                , adjLavaProb = 10
                                , wormProb = 33
                                , maxWormLength = 5
                                , lineOfSight = 4 
                                , maxWaterLevel = 20
                                , waterRefill = 15
                                , randGenSeed = 1337}
        defaultState = State {playerPos = [0,0]
                            , waterLevel = (maxWaterLevel defaultParam)
                            , tilesLooted = []
                            , tilesVisible = rmdups (detectTiles 0 0 [] (lineOfSight defaultParam))
                            , gameMap = []
                            , score = 0
                            , wormPaths = []
                            , gameMode = 0
                            , nextRand = 0
                            , availableThreads = []
                            , threadCount = 0
                            , gameStatus = 0 
                            , lastKeyStroke = (Char '_')
                            , param = defaultParam}
    initialState <- gameModeQuery defaultState
    let gameParam = (param initialState)
    if (lineOfSight gameParam) < 0 || (maxWaterLevel gameParam) < 0 || (waterRefill gameParam) < 0 || (treasureProb gameParam) < 0 || (waterProb gameParam) < 0 || (portalProb gameParam) < 0 || (lavaProb gameParam) < 0 || (adjLavaProb gameParam) < 0 || maxPathLength < 0 || renderWidth <= 0 || renderHeight <= 0 || (maxWormLength gameParam) <= 0 || (wormProb gameParam) < 0
        then do
            print "ERROR : negative parameters detected ! please provide only positive values for each parameter ! (renderWidth and renderHeight must be greater than 0 as well)"
            exitWith ExitSuccess
        else return ()
    if ((waterProb gameParam) + (portalProb gameParam) + (lavaProb gameParam) > 100) || ((waterProb gameParam) + (portalProb gameParam) + (adjLavaProb gameParam) > 100)
        then do
            print "ERROR : spawning probabilities sum is greater than 100 !"
            exitWith ExitSuccess
        else return ()
    let initialGameMap = generateMatrix gameParam
        initialState' = initialGameMap `seq` initialState {gameMap = initialGameMap}
    initialTVarState <- initialState' `seq` newTVarIO (initialState')
    createForkedThreads initialTVarState True (length (wormPaths initialState'))
    playIO (InWindow "Treasure Hunter" (1024, 600) (10, 10))
                            black
                            20
                            initialTVarState
                            renderWindow
                            getInput
                            processInput