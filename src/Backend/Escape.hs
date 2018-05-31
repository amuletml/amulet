{-# LANGUAGE OverloadedStrings #-}
module Backend.Escape
  ( EscapeScope
  , toLua, fromLua
  , createEscape, basicEscaper
  , pushVar, getVar, getEscaped
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Maybe
import Data.Char
import Data.Tuple

import Numeric (showHex)

import Text.Pretty.Semantic
import Core.Var

data EscapeScope = EscapeScope { toLua   :: Map.Map CoVar T.Text
                               , fromLua :: Map.Map T.Text CoVar
                               , escape  :: T.Text -> T.Text }

instance Show EscapeScope where
  show EscapeScope { toLua = l } = "EscapeScope " ++ show l

createEscape :: [(CoVar, T.Text)] -> (T.Text -> T.Text) -> EscapeScope
createEscape scope = EscapeScope (Map.fromList scope) (Map.fromList (map swap scope))

basicEscaper :: Set.Set T.Text -> T.Text -> T.Text
basicEscaper keywords name =
  let Just (t, ts) = T.uncons name
      esc = if isAlpha t && T.all (\x -> x == '_' || isAlphaNum x) ts
            then (if name `Set.member` keywords then T.cons '_' else id) name
            else T.concatMap escapeChar name
  in esc
  where
    escapeChar c | c > '\x75' = T.pack ('_':showHex (ord c) "")
                 | isAlpha c = T.singleton c
                 | otherwise = fromMaybe T.empty (Map.lookup c chars)

pushVar :: IsVar a => a -> EscapeScope -> (T.Text, EscapeScope)
pushVar v s = escapeVar (toVar v) where
  escapeVar v@(CoVar _ name _) =
    case Map.lookup v (toLua s) of
      Just _ -> error ("Variable already declared: " ++ show (pretty v))
      Nothing -> pushFirst Nothing (escape s name)

  pushFirst :: Maybe Int -> T.Text -> (T.Text, EscapeScope)
  pushFirst prefix esc =
    let esc' = esc <> maybe T.empty (T.pack . show) prefix
    in case Map.lookup esc' (fromLua s) of
         Nothing -> ( esc'
                    , s { fromLua = Map.insert esc' (toVar v) (fromLua s)
                        , toLua = Map.insert (toVar v) esc' (toLua s) })
         Just _ -> pushFirst (Just (maybe 0 (+1) prefix)) esc

getVar :: IsVar a => a -> EscapeScope -> T.Text
getVar v s = fromMaybe (error ("Cannot find " ++ show v)) (Map.lookup (toVar v) (toLua s))

getEscaped :: IsVar a => T.Text -> EscapeScope -> Maybe a
getEscaped v s = fromVar <$> Map.lookup v (fromLua s)

chars :: Map.Map Char T.Text
chars = Map.fromList
  [ (':', "_colon")
  , ('!', "_bang")
  , ('#', "_pound")
  , ('$', "_dollar")
  , ('%', "_percent")
  , ('&', "_amp")
  , ('*', "_star")
  , ('+', "_plus")
  , ('.', "_dot")
  , ('/', "_divide")
  , ('<', "_less")
  , ('=', "_equals")
  , ('>', "_greater")
  , ('?', "_ask")
  , ('@', "_at")
  , ('\\', "_slash")
  , ('^', "_hat")
  , ('|', "_bar")
  , ('-', "_minus")
  , ('~', "_tilde")
  , ('[', "_lbrack")
  , (']', "_rbrack ") ]
