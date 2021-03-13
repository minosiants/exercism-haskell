module Transpose (transpose) where

import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Trans.State (State, evalState, get, modify)
import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Debug.Trace

data TransState
  = TransState
      { source :: [String],
        widths :: [Int],
        rowsCount :: Int,
        currentRow :: String
      }
  deriving (Show)

mkState :: [String] -> TransState
mkState xs = TransState xs w 0 []
  where
    w = fmap length xs

incRowCount :: State TransState Int
incRowCount = do
  _ <- modify (\(TransState s w rc cv) -> TransState s w (rc + 1) cv)
  rowCountS

rowCountS :: State TransState Int
rowCountS = do rowsCount <$> get

sourceS :: State TransState [String]
sourceS = do source <$> get

currentRowS :: State TransState String
currentRowS = do currentRow <$> get

setCurrentRow :: String -> State TransState String
setCurrentRow v = do
    _ <- modify (\(TransState s w rc _) -> TransState s w rc v)
    currentRowS 
widthsS :: State TransState [Int]
widthsS = do widths <$> get

maxWidth :: State TransState Int
maxWidth = do maximum <$> widthsS

width :: Int -> State TransState (Maybe Int)
width i = do
  ws <- widthsS
  return $ if i >= length ws then Nothing else Just $ ws !! i

padded :: State TransState String
padded = do
  w <- maxWidth
  concatMap (\x -> take w (x ++ repeat ' ')) <$> sourceS

row :: Int -> String -> State TransState String
row i txt = do
  mw <- maxWidth
  _ <- incRowCount
  dropSpaces $ go mw (drop i txt)
  where
    go _ [] = ""
    go mw (x : xs) =
      x : go mw (drop (mw -1) xs)

dropSpaces :: String -> State TransState String
dropSpaces r = do
  let indexed = zip r [0 .. (length r -1)]
  _ <- setCurrentRow r
  r <- traverse addChar indexed
  let rr = foldr (<>) Nothing r
  return $ fromMaybe "" rr

addChar :: (Char, Int) -> State TransState (Maybe String)
addChar  (ch, i)
  | (not . isSpace) ch = return $ Just [ch]
  | otherwise = do
    c <- rowCountS
    w <- width i
    isLast <- checkIfLast i
    let r = w >>= (\w' -> if (w' >= c || not isLast) then Just [ch] else Nothing)
    return $ trace("count: " ++ show c++ " char: "++ show i)r



checkIfLast :: Int -> State TransState Bool
checkIfLast i = fmap (fromMaybe False) $ runMaybeT $ do
  w <- MaybeT $ width i
  cv <- lift currentRowS
  let v = drop i  
  ii <- lift (fmap length sourceS)
  r <- lift $ traverse width [i .. ii -1]
  t <- MaybeT . return $ sequence (tail r)
  let dd = (filter (not.isSpace) $ drop i cv) =="" 

  let res = not $ any (> w) t && (filter (not . isSpace) $ drop i cv) /=""  
  return $ trace ("width: " ++ show r ++ "row: \'" ++ cv++ "'  proc '"++show dd++"\'") res

transpose :: [String] -> [String]
transpose [] = []
transpose xs =
  evalState
    ( do
        p <- padded
        mw <- maxWidth
        traverse (`row` p) [0 .. (mw -1)]
    )
    (mkState xs)
