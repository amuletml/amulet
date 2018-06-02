module Repl.Display where

import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Data.Maybe

import qualified Foreign.Lua.Types.Error as L
import qualified Foreign.Lua.Api.Types as L
import qualified Foreign.Lua as L

import Text.Pretty.Semantic
import Text.Read (readMaybe)

data Value
  = String T.Text
  | Number (Either Integer Double)
  | Nil
  | Boolean Bool
  | Table (Map.Map T.Text Value)
  | Constructor T.Text (Maybe Value)
  | Opaque String -- a description of the opaque value
  deriving (Eq, Show, Ord)

valueRepr :: L.Lua () -> L.Lua Value
valueRepr getVal = do
  getVal
  t <- L.ltype L.stackTop
  case t of
    L.TypeString ->
      String . T.decodeLatin1
        <$> L.tostring L.stackTop
    L.TypeNumber -> do
      num <- L.tonumber L.stackTop
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
            weDo <- cont `L.catchLuaError` (const (pure False))
            if weDo
               then do
                 L.pushvalue (-2)
                 k <- T.decodeLatin1 <$> L.tostring L.stackTop
                 L.pop 1
                 v <- valueRepr (pure ())
                 Map.insert k v <$> loop cont
               else pure mempty
      tab <- loop (L.next table)
      pure $ if T.pack "__tag" `Map.member` tab
                then case tab Map.! T.pack "__tag" of
                       String x -> if x == T.pack "__builtin_unit"
                                      then Nil
                                      else Constructor x (Map.lookup (T.pack "1") tab)
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
  pretty (Constructor x Nothing) = stypeCon (text x)
  pretty (Constructor x (Just t)) = stypeCon (text x) <+> parensIf t where
    parensIf x@Constructor{} = parens (pretty x)
    parensIf x = pretty x
  pretty (Table m) | isTuple m = parens (hsep (punctuate comma (map pretty vs))) where
    vs = getValues m
    getValues m = case m Map.! T.pack "2" of
      Table m' | isTuple m' -> (m Map.! T.pack "1"):getValues m'
      _ -> [ m Map.! T.pack "1", m Map.! T.pack "2" ]
  pretty (Table m) = enclose (char '{' <> space) (space <> char '}') (hsep (punctuate comma vs)) where
    vs = map (\(k, v) -> text k <+> equals <+> pretty v) $ Map.toList m

isTuple :: Map.Map T.Text Value -> Bool
isTuple m =
  let keys = Map.keys m
   in length keys == 2 && all (\n -> isJust (readMaybe (T.unpack n) :: Maybe Integer)) keys

isInt :: L.LuaNumber -> Integer -> Bool
isInt x n = (round $ 10 ^ n * (x - fromIntegral (round x :: Integer))) == (0 :: Integer)
