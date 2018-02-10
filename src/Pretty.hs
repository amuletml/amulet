{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE DefaultSignatures, OverloadedStrings #-}
{-# LANGUAGE TypeApplications, ScopedTypeVariables #-}
module Pretty
  ( module M
  , PrettyM, PrettyP, PParam(..)
  , Pretty(..)
  , runPrinter, runPrettyM
  , defaults, colourless
  , prettyPrint, uglyPrint, prettyShow
  , ppshow
  , tracePretty, tracePrettyId
  , colour
  , block
  , newline
  , indented
  , body
  , typeClr
  , kwClr
  , tvClr
  , litClr
  , strClr
  , opClr
  , patClr
  , spcClr
  , greyOut
  , delim
  , width, width'
  , between
  , padRight
  , padRight'
  , bullet
  , Pretty.local
  , parens, braces, squares, angles
  , quotes, dquotes, verbatim
  , interleave
  , (<+>)
  , str
  , ppr
  ) where

import qualified Data.Map as Map

import Control.Monad.Writer.Strict as M
import Control.Monad.Reader as M hiding (local)
import Control.Applicative as M
import Data.Char as M
import Data.List as M
import Debug.Trace

import Data.Foldable

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Text (Text)

import qualified Control.Monad.Reader as RM

type PrettyM = ReaderT PParam (Writer Text)
type PrettyP = PrettyM ()

data PParam
  = PParam { colours        :: Bool
           , typeColour     :: Text
           , operatorColour :: Text
           , keywordColour  :: Text
           , patternColour  :: Text
           , stringColour   :: Text
           , typevarColour  :: Text
           , greyoutColour  :: Text
           , literalColour  :: Text
           , specialColour  :: Text
           , indent :: Int }
    deriving (Eq, Show)

class Pretty a where
  pprint :: a -> PrettyP
  default pprint :: Show a => a -> PrettyP
  pprint = tell . T.pack . show

runPrinter :: PParam -> PrettyP -> Text
runPrinter ctx act = let (_, ret) = runWriter (runReaderT act ctx) in ret

runPrettyM :: PParam -> PrettyM a -> (a, Text)
runPrettyM ct ac = runWriter (runReaderT ac ct)

defaults :: PParam
defaults = PParam { colours = True
                  , keywordColour  = "\x1b[35m"
                  , operatorColour = "\x1b[35m"
                  , literalColour  = "\x1b[1;33m"
                  , stringColour   = "\x1b[32m"
                  , typeColour     = "\x1b[34m"
                  , typevarColour  = "\x1b[36m"
                  , greyoutColour  = "\x1b[1;30m"
                  , patternColour  = "\x1b[35m"
                  , specialColour  = "\x1b[33m"
                  , indent = 0 }

colourless :: PParam
colourless = defaults { colours = False }

prettyPrint :: Pretty a => a -> Text
prettyPrint = runPrinter defaults . pprint

prettyShow :: Pretty a => a -> String
prettyShow = T.unpack . runPrinter defaults . pprint

uglyPrint :: Pretty a => a -> Text
uglyPrint = runPrinter colourless . pprint

ppshow :: Pretty a => PParam -> a -> Text
ppshow = (. pprint) . runPrinter

tracePretty :: Pretty a => a -> b -> b
tracePretty x = trace (T.unpack $ prettyPrint x)

tracePrettyId :: Pretty a => a -> a
tracePrettyId x = tracePretty x x

colour :: Pretty a => Text -> a -> PrettyP
colour clr cmb = do
  x <- asks colours
  when x (tell clr)
  pprint cmb <* when x (tell "\x1b[0m")

block :: Pretty a => Int -> a -> PrettyP
block st ac = RM.local (\x -> x { indent = indent x + st }) $ pprint ac

newline :: PrettyP
newline = do
  x <- asks indent
  tell "\n"
  tell $ T.replicate x " "

indented :: Pretty a => a -> PrettyP
indented x = newline *> pprint x

body :: Pretty a => Int -> [a] -> PrettyP
body _ [] = pure ()
body k b = block k $ traverse_ indented b

typeClr :: Pretty a => a -> PrettyP
typeClr x = flip colour x =<< asks typeColour

kwClr :: Pretty a => a -> PrettyP
kwClr x = flip colour x =<< asks keywordColour

tvClr :: Pretty a => a -> PrettyP
tvClr x = flip colour x =<< asks typevarColour

litClr :: Pretty a => a -> PrettyP
litClr x = flip colour x =<< asks literalColour

strClr :: Pretty a => a -> PrettyP
strClr x = flip colour x =<< asks stringColour

opClr :: Pretty a => a -> PrettyP
opClr x = flip colour x =<< asks operatorColour

patClr :: Pretty a => a -> PrettyP
patClr x = flip colour x =<< asks patternColour

spcClr :: Pretty a => a -> PrettyP
spcClr x = flip colour x =<< asks specialColour

greyOut :: Pretty a => a -> PrettyP
greyOut x = flip colour x =<< asks greyoutColour

delim :: (Pretty a, Pretty b) => a -> a -> b -> PrettyP
delim s e y = pprint s *> pprint y <* pprint e

width :: Pretty a => a -> PrettyM Int
width ac = do
  st <- asks indent
  let xs = runPrinter (colourless { indent = st }) $ pprint ac
   in return $ T.length xs

width' :: Pretty a => a -> PrettyM Int
width' ac = pprint ac *> width ac

between :: (Pretty a, Pretty b, Pretty c) => a -> b -> c -> PrettyP
between a b c = pprint a >> pprint c >> pprint b

padRight :: Pretty a => a -> Int -> PrettyM Text
padRight ac fl = do
  ct <- ask
  let xs = runPrinter ct $ pprint ac
   in return $ xs <> T.replicate (fl - T.length xs) " "

padRight' :: Pretty a => a -> Int -> PrettyP
padRight' ac fl = padRight ac fl >>= pprint

local :: Pretty a => a -> PrettyM Text
local ac = do
  ct <- ask
  let xs = runPrinter ct $ pprint ac
   in return xs

parens, braces, squares, angles :: Pretty a => a -> PrettyP
parens  = delim @Text "(" ")"
braces  = delim @Text "{ " " }"
squares = delim @Text "[" "]"
angles  = delim @Text "<" ">"

verbatim, quotes, dquotes :: Pretty a => a -> PrettyP
quotes  = delim @Text "'" "'"
dquotes = delim @Text "\"" "\""
verbatim = delim @Text "`" "`"

interleave :: (Pretty a, Pretty b) => b -> [a] -> PrettyP
interleave x xs = do
  env <- ask
  let x' = env `ppshow` x in
      tell $ T.intercalate x' $ map (ppshow env) xs

(<+>) :: (Pretty a, Pretty b) => a -> b -> PrettyP
a <+> b = pprint a >> pprint b
infixl 3 <+>

str :: Pretty a => a -> PrettyP
str = delim (greyOut @Text "\"") (greyOut @Text "\"") . strClr

ppr :: Pretty a => a -> IO ()
ppr = T.putStrLn . runPrinter defaults . pprint

bullet :: Pretty a => a -> PrettyP
bullet x = opClr @Text "· " <+> x

instance Pretty PrettyP where pprint = id
instance Pretty String where pprint = tell . T.pack
instance Pretty Char   where pprint = tell . T.singleton
instance Pretty Text   where pprint = tell
instance Pretty a => Pretty (ZipList a) where
  pprint (ZipList x) = interleave @a @Text ", " x

instance (Pretty a, Pretty b) => Pretty (Map.Map a b) where
  pprint mp = do
    x <- ask
    braces $ T.intercalate ", " $ map (\(k, v) -> x `ppshow` k <> " => " <> x `ppshow` v) $ Map.assocs mp

instance Pretty (a -> PrettyP, a) where
  pprint (x, y) = x y

instance Pretty Int
instance Pretty Double
instance Pretty Float
instance Pretty Integer
instance Pretty () where
  pprint _ = pure ()
