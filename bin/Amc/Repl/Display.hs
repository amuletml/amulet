{-# LANGUAGE OverloadedStrings #-}

module Amc.Repl.Display where

import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Maybe
import Data.Char

import qualified Foreign.Lua.Core.Error as L
import qualified Foreign.Lua as L

import Text.Pretty.Semantic

data Value
  = String T.Text
  | Number (Either Integer Double)
  | Nil
  | Boolean Bool
  | Table (Map.Map T.Text Value)
  | Constructor T.Text [Value]
  | Opaque String -- a description of the opaque value
  deriving (Eq, Show, Ord)

valueRepr :: L.Lua () -> L.Lua Value
valueRepr getVal = do
  getVal
  t <- L.ltype L.stackTop
  case t of
    L.TypeString ->
      String . T.decodeUtf8 . fromJust
        <$> L.tostring L.stackTop
    L.TypeNumber -> do
      num <- fromJust <$> L.tonumber L.stackTop
      pure . Number $ if isInt num 7
                         then Left (floor num)
                         else Right . fromRational . toRational $ num
    L.TypeNone -> error "Invalid stack index"
    L.TypeNil -> pure Nil
    L.TypeBoolean -> Boolean <$> L.toboolean L.stackTop
    L.TypeFunction -> pure (Opaque "function")
    L.TypeTable -> do
      table <- L.absindex L.stackTop
      L.pushnil
      let loop :: L.Lua Bool -> L.Lua (Map.Map T.Text Value)
          loop cont = do
            weDo <- cont `L.catchException` \e -> False <$ L.liftIO (print e)
            if weDo
               then do
                 L.pushvalue (-2)
                 k <- T.decodeLatin1 . fromJust <$> L.tostring L.stackTop
                 L.pop 1
                 v <- valueRepr (pure ())
                 Map.insert k v <$> loop cont
               else pure mempty
      tab <- loop (L.next table)
      pure $ if T.pack "__tag" `Map.member` tab
                then case tab ! T.pack "__tag" of
                       String x -> if x == T.pack "__builtin_unit"
                                      then Nil
                                      else Constructor x (map snd (Map.toList (Map.delete (T.pack "__tag") tab)))
                       _ -> error "Malformed constructor value when converting from Lua"
                else Table tab
    _ -> pure $ Opaque "foreign value"

    <* L.pop 1

instance Pretty Value where
  pretty (String x)  = sstring . dquotes . text $ x
  pretty (Number x)  = sliteral . either shown pretty $ x
  pretty Nil{}       = sliteral . string $ "()"
  pretty (Boolean x) = sliteral . string $ if x then "true" else "false"
  pretty (Opaque x)  = enclose (char '<') (char '>') (keyword x)

  pretty (Constructor "Cons" [Table m]) = p (m ! "_1") <+> soperator (string "::") <+> pretty (m ! "_2") where
    p x@(Constructor "Cons" _) = parens (pretty x)
    p x = pretty x
  pretty (Constructor "Nil" []) = brackets mempty

  pretty (Constructor "lazy" [x, _]) = stypeSkol (string "lazy") <+> pretty x

  pretty (Constructor x []) = stypeCon (text x)

  pretty (Constructor x ts) = colour (text x) <+> hsep (map parensIf ts) where
    parensIf x@Constructor{} = parens (pretty x)
    parensIf x = pretty x
    colour = if isLower (T.head x) then stypeSkol else stypeCon

  pretty (Table m) | isTuple m = parens (hsep (punctuate comma (map pretty vs))) where
    vs = getValues m
    getValues m = case m ! T.pack "_2" of
      Table m' | isTuple m' -> (m ! T.pack "_1"):getValues m'
      _ -> [ m ! T.pack "_1", m ! T.pack "_2" ]
  pretty (Table m) = enclose (char '{' <> space) (space <> char '}') (hsep (punctuate comma vs)) where
    vs = map (\(k, v) -> text k <+> equals <+> pretty v) $ Map.toList m

isTuple :: Map.Map T.Text Value -> Bool
isTuple m = Map.size m == 2 && "_1" `Map.member` m && "_2" `Map.member` m

isInt :: L.Number -> Integer -> Bool
isInt x n = round (10 ^ n * (x - fromIntegral (round x :: Integer))) == (0 :: Integer)

(!) :: Ord k => Map.Map k Value -> k -> Value
m ! k = fromMaybe Nil (Map.lookup k m)
