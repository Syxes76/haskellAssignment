import Control.Concurrent (threadDelay)
import System.IO
import Control.Monad (when)
import System.Exit
import System.Random

-- Player/State related functions

data State = State { xPos :: Int, yPos :: Int, waterLevel :: Int}
    deriving Show


movePlayerUp State { xPos = newXPos, yPos = newYPos, waterLevel = newWaterLevel } =
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

refillWater State { xPos = newXPos, yPos = newYPos, waterLevel = newWaterLevel } =
    State { xPos = newXPos, yPos = newYPos, waterLevel = 10 }

renderState :: State -> Int -> IO ()
renderState state score = do
    putStrLn    "=========================="
    putStr      "  Water units left : "
    print (waterLevel state)
    putStr      "  Treasures found  : "
    print score
    putStrLn    "=========================="


-- Map related functions

data Tile = Tile { tileType :: Char, hasTreasure :: Bool }
    deriving Show  

data RandomProp = RandomProp { randGen :: StdGen, treasureProb :: Int, waterProb :: Int, portalProb :: Int, lavaProb :: Int, lavaProb' :: Int }
    deriving Show  


randomTile :: Int -> Int -> RandomProp -> Int -> Tile
randomTile x y randomProp num = do
    let randNum = (randomRs (1,100) (randGen randomProp) :: [Int]) !! num
    if randNum <= (waterProb randomProp)
        then Tile { tileType = 'w', hasTreasure = False }
        else if randNum <= (waterProb randomProp) + (portalProb randomProp)
            then Tile { tileType = 'p', hasTreasure = False }
            else if randNum <= ((waterProb randomProp) + (portalProb randomProp) + (lavaProb randomProp))
                then Tile { tileType = 'l', hasTreasure = False }
                else do
                    let randNum = (randomRs (1,100) (randGen randomProp) :: [Int]) !! (num+1)
                    if randNum <= (treasureProb randomProp)
                        then Tile { tileType = 'd', hasTreasure = True }
                        else Tile { tileType = 'd', hasTreasure = False }

randomTile' :: Int -> Int -> RandomProp -> Int -> Tile
randomTile' x y randomProp num = do
    let randNum = (randomRs (1,100) (randGen randomProp) :: [Int]) !! num
    if randNum <= (waterProb randomProp)
        then Tile { tileType = 'w', hasTreasure = False }
        else if randNum <= (waterProb randomProp) + (portalProb randomProp)
            then Tile { tileType = 'p', hasTreasure = False }
            else if randNum <= ((waterProb randomProp) + (portalProb randomProp) + (lavaProb' randomProp))
                then Tile { tileType = 'l', hasTreasure = False }
                else do
                    let randNum = (randomRs (1,100) (randGen randomProp) :: [Int]) !! (num+1)
                    if randNum <= (treasureProb randomProp)
                        then Tile { tileType = 'd', hasTreasure = True }
                        else Tile { tileType = 'd', hasTreasure = False }

generateTile :: Int -> Int -> RandomProp -> [Tile] -> Int -> Tile
generateTile x y randomProp map num
    | x > 0 && y > 0 = do
        let adjYTile = getTile x (y-1) map
            adjXTile = getTile (x-1) y map
        if (tileType adjXTile) == 'l' || (tileType adjYTile) == 'l'
            then randomTile' x y randomProp num
            else randomTile x y randomProp num
    | x > 0 = do
        let adjXTile = getTile (x-1) y map
        if (tileType adjXTile) == 'l'
            then randomTile' x y randomProp num
            else randomTile x y randomProp num
    | y > 0 = do
        let adjYTile = getTile x (y-1) map
        if (tileType adjYTile) == 'l'
            then randomTile' x y randomProp num
            else randomTile x y randomProp num
    | otherwise = Tile { tileType = 'd', hasTreasure = False }


generateMatrix :: Int -> Int -> RandomProp -> [Tile] -> Int -> [Tile]
generateMatrix 0 0 randomProp map num = do
    let currTile = generateTile 0 0 randomProp map num 
    [currTile] ++ generateMatrix 1 0 randomProp (map ++ [currTile]) (num+2)
generateMatrix x 0 randomProp map num = do
    let currTile = generateTile x 0 randomProp map num 
    [currTile] ++ generateMatrix 0 x randomProp (map ++ [currTile]) (num+2)
generateMatrix 0 y randomProp map num = do
    let currTile = generateTile 0 y randomProp map num 
    [currTile] ++ generateMatrix y 1 randomProp (map ++ [currTile]) (num+2)
generateMatrix x y randomProp map num 
    | x > y = do
        let currTile = generateTile x y randomProp map num 
        [currTile] ++ generateMatrix y x randomProp (map ++ [currTile]) (num+2)
    | x < y = do
        let currTile = generateTile x y randomProp map num 
        [currTile] ++ generateMatrix y (x+1) randomProp (map ++ [currTile]) (num+2)
    | x == y = do
        let currTile = generateTile x y randomProp map num 
        [currTile] ++ generateMatrix (x+1) 0 randomProp (map ++ [currTile]) (num+2)

getTile :: Int -> Int -> [Tile] -> Tile
getTile 0 0 map = map!!0
getTile x 0 map = map!!(x*x)
getTile 0 y map = map!!((y*y)+1)
getTile x y map
    | x > y = map!!((x*x)+2*y)
    | x < y = map!!((y*y)+1+2*x)
    | x == y = map!!(((x+1)*(y+1))-1)

tileHasTreasure :: Int -> Int -> [Tile] -> Bool
tileHasTreasure x y map = do
    let currTile = getTile x y map
    if hasTreasure currTile
        then True
        else False

renderMap :: Int -> Int -> Int -> IO ()
renderMap lineOfSight x y = do
    putStrLn " _ _ _ _ _ _ _ _ _ "
    


-- Main loop functions
loop:: Int -> State -> [(Int,Int)] -> [(Int,Int)] -> [Tile] -> Int -> IO ()
loop lineOfSight state tilesExplored tilesSeen map score = do
    threadDelay 2000
    let currX = xPos state
        currY = yPos state
        currTile = getTile currX currY map
        currTileType = tileType currTile
    case currTileType of 
        'd' ->  do
            if (elem (currX,currY) tilesExplored) || not (tileHasTreasure currX currY map)
                then return()
                else do
                    let tilesExplored' = tilesExplored ++ [(currX,currY)]
                    loop lineOfSight state tilesExplored' tilesSeen map (score+1)
        'l' ->  do
            print "YOU FELL IN LAVA ! GAME OVER !"
            exitWith ExitSuccess
        'p' ->  do
            putStr "YOU WIN ! YOUR SCORE WAS " 
            print score
            exitWith ExitSuccess
        'w' ->  do
            if elem (currX,currY) tilesExplored
                then return()
                else do
                    let tilesExplored' = tilesExplored ++ [(currX,currY)]
                        state' = refillWater state
                    loop lineOfSight state' tilesExplored' tilesSeen map score
    if waterLevel state < 1
        then do
            print "YOU RAN OUT OF WATER ! GAME OVER !"
            exitWith ExitSuccess
        else do
            renderState state score
            key <- getChar
            when (key /= 'k') $ do
                case key of
                    'z' -> do 
                        let state' = movePlayerUp state
                        print "UP"
                        loop lineOfSight state' tilesExplored tilesSeen map score
                    's' -> do 
                        let state' = movePlayerDown state
                        print "DOWN"
                        loop lineOfSight state' tilesExplored tilesSeen map score
                    'q' -> do 
                        let state' = movePlayerLeft state
                        print "LEFT"
                        loop lineOfSight state' tilesExplored tilesSeen map score
                    'd' -> do 
                        let state' = movePlayerRight state
                        print "RIGHT"
                        loop lineOfSight state' tilesExplored tilesSeen map score
                    _   -> do
                        let state' = state
                        loop lineOfSight state' tilesExplored tilesSeen map score


main = do
    let initialState = State {xPos = 0, yPos = 0, waterLevel = 10 }
        randGen' = mkStdGen 1337
        randomProp = RandomProp { randGen = randGen', treasureProb = 40, waterProb = 30, portalProb = 10, lavaProb = 30, lavaProb' = 20 }
        lineOfSight = 4
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    let initialMap = generateMatrix 0 0 randomProp [] 1
    loop lineOfSight initialState [] initialMap 0
