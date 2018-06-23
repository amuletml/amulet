{-# LANGUAGE OverloadedStrings #-}

{-| Handles converting Amulet identifiers into a valid identifier in
  various languages. This also keeps track of variables in scope,
  preventing shadowed variables being given identical names.
-}
module Backend.Escape
  ( EscapeScope
  , toEsc, fromEsc
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

-- | A two-way mapping of 'CoVar's to their escaped representation.
data EscapeScope = EscapeScope { toEsc   :: Map.Map CoVar T.Text -- ^ The mapping of core variables to escaped ones
                               , fromEsc :: Map.Map T.Text CoVar -- ^ The mapping of escaped variables to core ones
                               , escape  :: T.Text -> T.Text     -- ^ The function responsible for escaping vars
                               }

instance Show EscapeScope where
  show EscapeScope { toEsc = l } = "EscapeScope " ++ show l

-- | Create an escape scope from a list of builtin-in variables and some
-- escaping function.
createEscape :: [(CoVar, T.Text)] -> (T.Text -> T.Text) -> EscapeScope
createEscape scope = EscapeScope (Map.fromList scope) (Map.fromList (map swap scope))

-- | A basic escaping function, which only allows alpha-numeric
-- characters and underscores in identifiers.
basicEscaper :: Set.Set T.Text -- ^ A set of keywords. Namely, invalid identifiers that
                               -- the escaper should not emit
             -> T.Text -- ^ The variable to escape
             -> T.Text -- ^ The escaped string
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

-- | Push a variable into the escape scope, yielding the escaped name and
-- the new scope.
pushVar :: IsVar a => a -> EscapeScope -> (T.Text, EscapeScope)
pushVar v s = escapeVar (toVar v) where
  escapeVar v@(CoVar _ name _) =
    case Map.lookup v (toEsc s) of
      Just _ -> error ("Variable already declared: " ++ show (pretty v))
      Nothing -> pushFirst Nothing (escape s name)

  pushFirst :: Maybe Int -> T.Text -> (T.Text, EscapeScope)
  pushFirst prefix esc =
    let esc' = esc <> maybe T.empty (T.pack . show) prefix
    in case Map.lookup esc' (fromEsc s) of
         Nothing -> ( esc'
                    , s { fromEsc = Map.insert esc' (toVar v) (fromEsc s)
                        , toEsc = Map.insert (toVar v) esc' (toEsc s) })
         Just _ -> pushFirst (Just (maybe 0 (+1) prefix)) esc

-- | Look up the escaped representation of a variable. This is a partial
-- function and will error if it does not exist.
getVar :: IsVar a => a -> EscapeScope -> T.Text
getVar v s = fromMaybe (error ("Cannot find " ++ show v)) (Map.lookup (toVar v) (toEsc s))

-- | Look up the variable for a given input string.
getEscaped :: IsVar a => T.Text -> EscapeScope -> Maybe a
getEscaped v s = fromVar <$> Map.lookup v (fromEsc s)

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
  , (']', "_rbrack ")
  , ('\'', "_prime")
  , ('_', "_") ]
