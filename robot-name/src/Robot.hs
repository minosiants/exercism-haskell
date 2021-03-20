module Robot (Robot, initialState, mkRobot, resetName, robotName) where

import Control.Monad (replicateM)
import Control.Monad.State
import Control.Monad.Trans (lift)
import Data.IORef
import qualified Data.Set as S
import Data.Set
import System.Random (Random (randomRIO))

type RunStateIO = StateT RunState IO

type Name = String

newtype Robot = Robot {name :: (IORef Name)} deriving (Eq)

newtype RunState = RunState {names :: Set Name} deriving (Show, Eq)

initialState :: RunState
initialState = RunState empty

mkRobot :: RunStateIO Robot
mkRobot = do
  newName <- uniqueName
  noname <- lift $ newIORef newName
  return $ Robot noname

resetName :: Robot -> RunStateIO ()
resetName robot = do
  names' <- gets names
  newName <- uniqueName
  oldName <- lift $ robotName robot
  let res = delete oldName names'
  put (RunState (insert newName res))

robotName :: Robot -> IO String
robotName robot = readIORef $ name robot

uniqueName :: RunStateIO String
uniqueName = do
  newName <- lift nameGen
  n <- gets names
  if newName `elem` n
    then uniqueName
    else return newName

nameGen :: IO String
nameGen = do
  num <- (randomRIO (100 :: Int, 999))
  pref <- replicateM 2 (randomRIO ('A', 'Z'))
  return $ pref ++ show num
