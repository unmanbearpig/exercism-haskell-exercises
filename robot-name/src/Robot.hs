module Robot (Robot, mkRobot, resetName, robotName) where

import Data.IORef
import System.Random

newtype Robot = Robot { nameRef :: IORef String }

namePattern :: [(Char, Char)]
namePattern = concat $ uncurry replicate <$> [(2, ('A', 'Z')), (3, ('0', '9'))]

mkName :: IO String
mkName = sequence $ randomRIO <$> namePattern

mkRobot :: IO Robot
mkRobot = Robot <$> (mkName >>= newIORef)

resetName :: Robot -> IO ()
resetName robot = mkName >>= writeIORef (nameRef robot)

robotName :: Robot -> IO String
robotName = readIORef . nameRef
