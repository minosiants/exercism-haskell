module ETL (transform) where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as M

transform :: Map a String -> Map Char a
transform =
  M.foldrWithKey
    ( \k v acc ->
        M.union acc (M.fromList $ fmap (\l -> (toLower l, k)) v)
    )
    M.empty
