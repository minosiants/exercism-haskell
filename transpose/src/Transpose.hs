module Transpose (transpose,testTranspose) where

import Control.Monad.Trans.State
import Data.Char (isSpace)
import Data.List (dropWhileEnd, find)
import Data.Maybe ( fromMaybe )
import Debug.Trace

transpose' :: [String] -> [String]
transpose' [] = []
transpose' xs =
  fmap (dropWhileEnd (== ' ') . (`column` padded)) [0 .. width -1]
  where
    width = maximum $ fmap length xs
    padded =
      concatMap (\x -> take width (x ++ repeat ' ')) xs
    column i txt = go (drop i txt)
      where
        go [] = ""
        go (x : xs') = x : go (drop (width -1) xs')

data TransState
  = TransState
      { source :: [String],
        widths :: [Int],
        rowsCount :: Int,
        result :: [String]
      }
  deriving (Show)

mkState :: [String] -> TransState
mkState xs = TransState xs w 0 []
  where
    w = fmap length xs

incRowCount :: State TransState Int
incRowCount = do
  _ <- modify (\(TransState s w rc c) -> TransState s w (rc + 1) c)
  rowCountS

rowCountS :: State TransState Int
rowCountS = do rowsCount <$> get

sourceS :: State TransState [String]
sourceS = do source <$> get

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
  r  <- dropSpaces $  go mw (drop i txt) 
  _ <- incRowCount
  return r
  where
    go _ [] = ""
    go mw (x : xs) =
      x : go mw (drop (mw -1) xs)

dropSpaces :: String -> State TransState String
dropSpaces r = do
  let indexed = zip  r  [0 .. (length r -1)]
  r <- traverse addChar indexed
  let rr = foldr (<>) Nothing r
  return $ fromMaybe "" rr

addChar :: (Char, Int) -> State TransState (Maybe String)
addChar (ch, i) = do
  c <- rowCountS
  w <- width i
  isLast <- checkIfLast i
  let r = w >>= (\w' -> if (w' > c || (isLast && (not . isSpace) ch)) then Just [ch] else Nothing)
  return r 

checkIfLast :: Int -> State TransState Bool 
checkIfLast i = do
    ii <- fmap length sourceS 
    w <- width i
    r <- traverse width  [i..ii-1]
    let t= tail r   
    let res = not $ any (\x -> fromMaybe 0 x > fromMaybe 0 w ) t
    return res 
  

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



testTranspose :: IO ()
testTranspose = do
  putStr "start"
  let txt = ["TAA AT","M r", "Rtyr"]
  let res = transpose txt 
  putStr "before\n"
  putStr $ show res
  putStr "\nafter\n"
  putStr "end"