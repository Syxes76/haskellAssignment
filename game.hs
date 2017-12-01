import Control.Concurrent (threadDelay)
import System.IO
import Control.Monad (when)
import System.Exit
import System.Random
import Data.List
import System.Console.ANSI

maxPathLength = 10

-- These values control how the map is rendered
-- The double of this value plus 1 represents the amount of tiles rendered horizontaly
renderWidth = 9
-- The double of this value plus 1 represents the amount of tiles rendered verticaly
renderHeight = 8

-- For a terminal with a resolution of 75x37, recommended values are :
-- renderWidth = 9
-- renderHeight = 8

-- Misc function for removing duplicates
-- from StackOverflow
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort


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

renderState :: State -> Int -> [(Int,Int)] -> [Tile] -> IO ()
renderState state score tilesLooted map = do
    putStrLn    "=========================================================================="
    putStrLn      ("  Water units left : " ++ (show (waterLevel state)) ++ "   Treasures found  : " ++ (show score))
    let waterDist = getClosestTile (xPos state) (yPos state) 'w' tilesLooted map
        portalDist = getClosestTile (xPos state) (yPos state) 'p' tilesLooted map
        treasureDist = getClosestTile (xPos state) (yPos state) 'd' tilesLooted map
    if waterDist == maxPathLength*2
        then putStr "  Nearest water : 10+"
        else putStr ("  Nearest water : " ++ (show waterDist))
    if portalDist == maxPathLength*2
        then putStr "   Nearest portal : 10+"
        else putStr ("  Nearest portal : " ++ (show portalDist))
    if treasureDist == maxPathLength*2
        then putStrLn "   Nearest treasure : 10+"
        else putStrLn ("  Nearest treasure : " ++ (show treasureDist))
    putStrLn    "=========================================================================="


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

generateMatrix :: RandomProp -> [Tile]
generateMatrix randomProp = generateMatrix' 0 0 randomProp [] 1


generateMatrix' :: Int -> Int -> RandomProp -> [Tile] -> Int -> [Tile]
generateMatrix' 0 0 randomProp map num = do
    let currTile = generateTile 0 0 randomProp map num 
    [currTile] ++ generateMatrix' 1 0 randomProp (map ++ [currTile]) (num+2)
generateMatrix' x 0 randomProp map num = do
    let currTile = generateTile x 0 randomProp map num 
    [currTile] ++ generateMatrix' 0 x randomProp (map ++ [currTile]) (num+2)
generateMatrix' 0 y randomProp map num = do
    let currTile = generateTile 0 y randomProp map num 
    [currTile] ++ generateMatrix' y 1 randomProp (map ++ [currTile]) (num+2)
generateMatrix' x y randomProp map num 
    | x > y = do
        let currTile = generateTile x y randomProp map num 
        [currTile] ++ generateMatrix' y x randomProp (map ++ [currTile]) (num+2)
    | x < y = do
        let currTile = generateTile x y randomProp map num 
        [currTile] ++ generateMatrix' y (x+1) randomProp (map ++ [currTile]) (num+2)
    | x == y = do
        let currTile = generateTile x y randomProp map num 
        [currTile] ++ generateMatrix' (x+1) 0 randomProp (map ++ [currTile]) (num+2)


getTile :: Int -> Int -> [Tile] -> Tile
getTile 0 0 map = map!!0
getTile x 0 map = do
    if x > 0
        then map!!(x*x)
        else Tile { tileType = 'x', hasTreasure = False }
getTile 0 y map = do
    if y > 0
        then map!!((y*y)+1)
        else Tile { tileType = 'x', hasTreasure = False }
getTile x y map
    | x < 0 || y < 0 = Tile { tileType = 'x', hasTreasure = False }
    | x > y = map!!((x*x)+2*y)
    | x < y = map!!((y*y)+1+2*x)
    | x == y = map!!(((x+1)*(y+1))-1)


tileHasTreasure :: Int -> Int -> [Tile] -> Bool
tileHasTreasure x y map = do
    let currTile = getTile x y map
    if hasTreasure currTile
        then True
        else False


detectTiles :: Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)]
detectTiles lineOfSight x y tilesVisible = detectTiles' lineOfSight x y 0 0 tilesVisible


detectTiles' :: Int -> Int -> Int -> Int -> Int -> [(Int,Int)] -> [(Int,Int)]
detectTiles' lineOfSight x y nx ny tilesVisible
    | ny == (lineOfSight*2) = do
        if not (elem (x,(y+lineOfSight)) tilesVisible)
            then [(x,(y+lineOfSight))]
            else []
    | ny >= 0 && ny < lineOfSight = do
        if (nx <= ny)
            then [((x+nx),(y-(lineOfSight-ny)))] ++ detectTiles' lineOfSight x y (nx+1) ny tilesVisible
            else [] ++ detectTiles' lineOfSight x y (negate (ny+1)) (ny+1) tilesVisible
    | ny >= lineOfSight && ny < (lineOfSight*2) = do
        if (nx <= (lineOfSight-(mod ny lineOfSight)))
            then [((x+nx),(y+(ny-lineOfSight)))] ++ detectTiles' lineOfSight x y (nx+1) ny tilesVisible
            else [] ++ detectTiles' lineOfSight x y (negate (lineOfSight-(mod (ny+1) lineOfSight))) (ny+1) tilesVisible


renderMap :: Int -> Int -> Int -> [(Int,Int)] -> [Tile] -> IO ()
renderMap lineOfSight x y tilesVisible map = renderMap' lineOfSight x y (negate (renderWidth*2)) (negate (renderHeight*2)) tilesVisible map


renderMap' :: Int -> Int -> Int -> Int -> Int -> [(Int,Int)] -> [Tile] -> IO ()
renderMap' lineOfSight x y nx ny tilesVisible map
    | nx == 0 && ny == 0 = do
        putStr "|P"
        renderMap' lineOfSight x y (nx+1) ny tilesVisible map
    | otherwise = do
        if nx < (renderWidth*2) 
            then do
                if elem ((x+nx),(y+ny)) tilesVisible
                    then do
                        let currTile = getTile (x+nx) (y+ny) map
                            currTileType = tileType currTile
                        case currTileType of 
                            'd' ->  putStr "| "
                            'l' ->  putStr "|!"
                            'p' ->  putStr "|O"
                            'w' ->  putStr "|~"
                            _   ->  putStr "|X"
                    else putStr "|?"
                renderMap' lineOfSight x y (nx+1) ny tilesVisible map
            else do
                if elem ((x+nx),(y+ny)) tilesVisible
                    then do
                        let currTile = getTile (x+nx) (y+ny) map
                            currTileType = tileType currTile
                        case currTileType of 
                            'd' ->  putStrLn "| |"
                            'l' ->  putStrLn "|!|"
                            'p' ->  putStrLn "|O|"
                            'w' ->  putStrLn "|~|"
                            _   ->  putStrLn "|X|"
                    else putStrLn "|?|"
                if ny < (renderHeight*2)
                    then do
                        renderMap' lineOfSight x y (negate (renderWidth*2)) (ny+1) tilesVisible map
                    else
                        return ()


getClosestTile :: Int -> Int -> Char -> [(Int,Int)] -> [Tile] -> Int
getClosestTile sx sy targetTile tilesLooted map = getClosestTile' sx sy targetTile 0 tilesLooted [] map


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


-- Main loop functions
loop:: Int -> State -> [(Int,Int)] -> [(Int,Int)] -> [Tile] -> Int -> IO ()
loop lineOfSight state tilesLooted tilesVisible map score = do
    threadDelay 2000
    let currX = xPos state
        currY = yPos state
        currTile = getTile currX currY map
        currTileType = tileType currTile
    case currTileType of 
        'd' ->  do
            if (elem (currX,currY) tilesLooted) || not (tileHasTreasure currX currY map)
                then return()
                else do
                    let tilesLooted' = tilesLooted ++ [(currX,currY)]
                    loop lineOfSight state tilesLooted' tilesVisible map (score+1)
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
                    loop lineOfSight state' tilesLooted' tilesVisible map score
    if waterLevel state < 1
        then do
            print "YOU RAN OUT OF WATER ! GAME OVER !"
            exitWith ExitSuccess
        else do
            clearScreen 
            let newTilesVisible = tilesVisible ++ (detectTiles lineOfSight currX currY tilesVisible)
                tilesVisible' = rmdups newTilesVisible
            renderMap lineOfSight currX currY tilesVisible' map
            renderState state score tilesLooted map
            key <- getChar
            case key of
                'k' -> do 
                    print "YOU KILLED YOURSELF ! GAME OVER !"
                    exitWith ExitSuccess
                'z' -> do 
                    let state' = movePlayerUp state
                    loop lineOfSight state' tilesLooted tilesVisible' map score
                's' -> do 
                    let state' = movePlayerDown state
                    loop lineOfSight state' tilesLooted tilesVisible' map score
                'q' -> do 
                    let state' = movePlayerLeft state
                    loop lineOfSight state' tilesLooted tilesVisible' map score
                'd' -> do 
                    let state' = movePlayerRight state
                    loop lineOfSight state' tilesLooted tilesVisible' map score
                _   -> do
                    let state' = state
                    loop lineOfSight state' tilesLooted tilesVisible' map score


main = do
    if renderWidth < 0 || renderHeight < 0 
        then do
            print "ERROR : renderWidth and renderHeight must be greater than 0"
        else return ()
    let initialState = State {xPos = 0, yPos = 0, waterLevel = 10 }
        randGen' = mkStdGen 1337
        randomProp = RandomProp { randGen = randGen', treasureProb = 40, waterProb = 30, portalProb = 10, lavaProb = 0, lavaProb' = 0 }
        lineOfSight = 4
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    let initialMap = generateMatrix randomProp
    loop lineOfSight initialState [] [] initialMap 0
