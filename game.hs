import Control.Concurrent (threadDelay)
import System.IO
import Control.Monad (when)

data State = State { xPos :: Int, yPos :: Int, waterLevel :: Int}
    deriving Show

data Tile = Tile { tileXPos :: Int, tileYPos :: Int, tileType :: Char }
    deriving Show

checkPortal :: State -> [Tile] -> Bool
checkPortal state map = False

checkWater :: State -> Bool
checkWater state
    | waterLevel state > 0  = True
    | otherwise             = False
    

loop :: State -> [(Int,Int)] -> [(Int,Int)] -> [Tile] -> IO ()
loop state treasureFound waterFound map
    | checkPortal state map = print "YOU MADE IT OUT ALIVE !"
    | checkWater state =
        do
            threadDelay 2000
            let currX = xPos state
                currY = yPos state
                currTile = getTile currX currY map
                currTileType = tileType currTile
            case currTileType of 
                'd' -> 
                'l' -> 
                'p' ->
                'w' -> 
            let state' = state
            key <- getChar
            when (key /= 'k') $ do
                case key of
                    'z' -> do 
                        let state'' = movePlayerUp state'
                        print "UP"
                        loop state'' treasureFound waterFound map
                    's' -> do 
                        let state'' = movePlayerDown state'
                        print "DOWN"
                        loop state'' treasureFound waterFound map
                    'q' -> do 
                        let state'' = movePlayerLeft state'
                        print "LEFT"
                        loop state'' treasureFound waterFound map
                    'd' -> do 
                        let state'' = movePlayerRight state'
                        print "RIGHT"
                        loop state'' treasureFound waterFound map
                    _   -> do
                        let state'' = state'
                        loop state'' treasureFound waterFound map
    | otherwise = print "GAME OVER"


main = do
    let initialState = State {xPos = 0, yPos = 0, waterLevel = 10 }
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    let initialMap = generateMatrix 0 0
    loop initialState [] [] initialMap


    
movePlayerUp State { xPos = newXPos, yPos = newYPos, waterLevel = newWaterLevel } =
    if newXPos > 0
        then State { xPos = newXPos, yPos = newYPos - 1, waterLevel = newWaterLevel - 1 }
        else State { xPos = newXPos, yPos = newYPos, waterLevel = newWaterLevel }

movePlayerDown State { xPos = newXPos, yPos = newYPos, waterLevel = newWaterLevel } = 
    State { xPos = newXPos, yPos = newYPos + 1, waterLevel = newWaterLevel - 1 }

movePlayerLeft State { xPos = newXPos, yPos = newYPos, waterLevel = newWaterLevel } = 
    if newYPos > 0
        then State { xPos = newXPos - 1, yPos = newYPos, waterLevel = newWaterLevel - 1 }
        else State { xPos = newXPos, yPos = newYPos, waterLevel = newWaterLevel }

movePlayerRight State { xPos = newXPos, yPos = newYPos, waterLevel = newWaterLevel } = 
    State { xPos = newXPos + 1, yPos = newYPos, waterLevel = newWaterLevel - 1 }


generateMatrix :: Int -> Int -> [Tile]
generateMatrix 0 0 = [Tile { tileXPos = 0, tileYPos = 0, tileType = 'd' }] ++ generateMatrix 1 0
generateMatrix x 0 = [Tile { tileXPos = x, tileYPos = 0, tileType = 'd' }] ++ generateMatrix 0 x
generateMatrix 0 y = [Tile { tileXPos = 0, tileYPos = y, tileType = 'd' }] ++ generateMatrix y 1
generateMatrix x y
    | x > y = [Tile { tileXPos = x, tileYPos = y, tileType = 'd' }] ++ generateMatrix y x
    | x < y = [Tile { tileXPos = x, tileYPos = y, tileType = 'd' }] ++ generateMatrix y (x+1)
    | x == y = [Tile { tileXPos = x, tileYPos = y, tileType = 'd' }] ++ generateMatrix (x+1) 0

getTile :: Int -> Int -> [Tile] -> Tile
getTile 0 0 map = map!!0
getTile x 0 map = map!!(x*x)
getTile 0 y map = map!!((y*y)+1)
getTile x y map
    | x > y = map!!((x*x)+2*y)
    | x < y = map!!((y*y)+1+2*x)
    | x == y = map!!(((x+1)*(y+1))-1)