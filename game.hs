import Control.Concurrent (threadDelay)
import System.IO
import Control.Monad (when)

data State = State { x :: Int, y :: Int, w :: Int }

loop :: State -> IO ()
loop state = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    let newState = state
    key <- getChar
    when (key /= 'k') $ do
        case key of
            'z' -> do 
                let newState = movePlayerUp state
                print "UP"
            's' -> do 
                let newState = movePlayerDown state
                print "DOWN"
            'q' -> do 
                let newState = movePlayerLeft state
                print "LEFT"
            'd' -> do 
                let newState = movePlayerRight state
                print "RIGHT"
            _   -> return ()
        threadDelay 2000
        loop newState

main = do
    let initialState = State {x = 0, y = 0, w = 10}
    loop initialState

movePlayerUp State { x = xpos, y = ypos, w = water } =
    if xpos > 0
        then State { x = xpos - 1, y = ypos, w = water - 1 }
        else State { x = xpos, y = ypos, w = water }

movePlayerDown State { x = xpos, y = ypos, w = water } = State { x = xpos + 1, y = ypos, w = water - 1 }

movePlayerLeft State { x = xpos, y = ypos, w = water } = 
    if ypos > 0
        then State { x = xpos, y = ypos + 1, w = water - 1 }
        else State { x = xpos, y = ypos, w = water }

movePlayerRight State { x = xpos, y = ypos, w = water } = State { x = xpos, y = ypos - 1, w = water - 1 }