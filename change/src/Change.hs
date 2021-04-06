module Change (findFewestCoins) where

import Data.List
import Debug.Trace
import Control.Applicative
import Control.Monad

import Data.Maybe



findCoins :: Integer ->[Integer] -> Maybe [Integer]
findCoins _ [] = Nothing
findCoins 0 _ = Just []
findCoins target coins  = do
            coin <- findCoin target coins'
            let diff  = target - coin
            let diff' = trace ("diff " ++ show diff ++  " coin "++ show coin ++ " coins" ++ show coin ++ " coins' "++ show coins'++ " target " ++ show target) diff
            c <- if diff == 0 then pure [] else  findCoins diff' coins'
            return $ coin : c
    where
        coins' = filter (<=target) coins

findCoin :: Integer -> [Integer] -> Maybe Integer
findCoin _ [] = Nothing
findCoin target coins =
           let coins' = filter (<= target) coins
               candidate  = last coins'
               diff = target - candidate
               check =  filter (<= diff) coins
               msg = trace("coins' "++ show coins' ++ " candidate "  ++ show candidate ++ " diff "++ show diff++" check "++ show check)(check)
            in
                if null msg  && diff /= 0
                then findCoin target (init coins')
                else Just candidate


findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins _ [] =Nothing
findFewestCoins target coins =
    let
        rest = findFewestCoins target (init coins)
        allCases = sequence $ filter isJust $ findCoins target coins : [rest]
        v = do
          ac <- allCases
          let r = fmap snd $ sortOn fst $fmap (\x -> (length x, x)) ac 
          if null r then Nothing else return $ head r  

    in v

