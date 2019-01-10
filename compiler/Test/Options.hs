module Test.Options
  ( parsePrefix
  ) where

import qualified Data.Map as Map
import Data.List

data Prefixed a = Known String a | Ambiguous [String]

type PrefixMap a = Map.Map String (Prefixed a)

insertPrefixed :: String -> a -> PrefixMap a -> PrefixMap a
insertPrefixed k v m = foldr (Map.alter (Just . go)) m (inits k) where
  go Nothing = Known k v
  go (Just (Known k' _)) = Ambiguous [k, k']
  go (Just (Ambiguous ks)) = Ambiguous (k:ks)

parsePrefix :: [(String, a)] -> String -> Maybe a
parsePrefix os = maybePrefixed . flip Map.lookup prefixMap where
  prefixMap = foldr (uncurry insertPrefixed) mempty os

  maybePrefixed Nothing = Nothing
  maybePrefixed (Just Ambiguous{}) = Nothing
  maybePrefixed (Just (Known _ x)) = Just x
