{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Pretty 
  ( module M
  , PrettyM, PrettyP, PParam(..), PState(..)
  , Pretty(..)
  , runPrinter, runPrinter', runPrettyM
  , defaults, colourless
  , prettyPrint, uglyPrint
  , ppshow
  , colour
  , block
  , step
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
  , width
  , between
  , padRight
  , padRight'
  , Pretty.local
  , parens, braces, squares, angles
  , quotes, dquotes
  , interleave
  , (<+>)
  , str
  , ppr
  ) where

import qualified Data.Map as Map

import Control.Monad.RWS.Strict as M hiding (local)
import Control.Applicative as M
import Data.List as M
import Data.Char as M

type PrettyM = RWS PParam String PState
type PrettyP = PrettyM ()

data PParam
  = PParam { colours        :: Bool
           , typeColour     :: String
           , operatorColour :: String
           , keywordColour  :: String
           , patternColour  :: String
           , stringColour   :: String
           , typevarColour  :: String
           , greyoutColour  :: String
           , literalColour  :: String
           , specialColour  :: String }
    deriving (Eq, Show)

newtype PState
  = PState { indent :: Int }
  deriving (Eq, Show, Ord)

class Pretty a where
  pprint :: a -> PrettyP
  default pprint :: Show a => a -> PrettyP
  pprint = tell . show

runPrinter :: PParam -> PrettyP -> String
runPrinter ctx act = let (_, _, ret) = runRWS act ctx (PState 0) in ret

runPrinter' :: PParam -> PState -> PrettyM a -> String
runPrinter' ct st ac = let (_, _, ret) = runRWS ac ct st in ret

runPrettyM :: PParam -> PState -> PrettyM a -> (a, PState, String)
runPrettyM ct st ac = runRWS ac ct st

defaults :: PParam
defaults = PParam { colours = True
                  , keywordColour  = "\x1b[35m"
                  , operatorColour = "\x1b[35m"
                  , literalColour  = "\x1b[1;33m"
                  , stringColour   = "\x1b[32m"
                  , typeColour     = "\x1b[34m"
                  , typevarColour  = "\x1b[0m"
                  , greyoutColour  = "\x1b[1;30m"
                  , patternColour  = "\x1b[35m"
                  , specialColour  = "\x1b[33m" }

colourless :: PParam
colourless = defaults { colours = False }

prettyPrint :: Pretty a => a -> String
prettyPrint = runPrinter defaults . pprint

uglyPrint :: Pretty a => a -> String
uglyPrint = runPrinter colourless . pprint

ppshow :: Pretty a => PParam -> a -> String
ppshow = (. pprint) . runPrinter

colour :: Pretty a => String -> a -> PrettyP
colour clr cmb
  = do x <- asks colours
       when x (tell clr)
       y <- pprint cmb
       when x $ tell "\x1b[0m"
       return y

block :: Pretty a => Int -> a -> PrettyP
block st ac
  = do x <- gets indent
       put $ PState (x + st)
       pprint ac
       put $ PState x

step :: Int -> PrettyP
step x = modify $ \s -> s { indent = indent s + x }

newline :: PrettyP
newline
  = do x <- gets indent
       tell "\n"
       tell $ replicate x ' '

indented :: Pretty a => a -> PrettyP
indented x = newline *> pprint x

body :: Pretty a => [a] -> PrettyP
body b = block 1 $ mapM_ indented b

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

delim :: (Pretty a, Pretty b, Pretty c) => a -> b -> c -> PrettyP
delim s e y = pprint s *> pprint y <* pprint e

width :: Pretty a => a -> PrettyM Int
width ac = do st  <- get
              let xs = runPrinter' colourless st $ pprint ac
               in return $ length xs

between :: (Pretty a, Pretty b, Pretty c) => a -> b -> c -> PrettyP
between a b c = pprint a >> pprint c >> pprint b

padRight :: Pretty a => a -> Int -> PrettyM String
padRight ac fl
  = do st <- get
       ct <- ask
       let xs = runPrinter' ct st $ pprint ac
        in return $ xs ++ replicate (fl - length xs) ' '

padRight' :: Pretty a => a -> Int -> PrettyP
padRight' ac fl = padRight ac fl >>= pprint

local :: Pretty a => a -> PrettyM String
local ac
  = do st <- get
       ct <- ask
       let xs = runPrinter' ct st $ pprint ac
        in return xs

parens, braces, squares, angles :: Pretty a => a -> PrettyP
parens  = delim "(" ")"
braces  = delim "{" "}"
squares = delim "[" "]"
angles  = delim "<" ">"

quotes, dquotes :: Pretty a => a -> PrettyP
quotes  = delim "'" "'"
dquotes = delim "\"" "\""

interleave :: (Pretty a, Pretty b) => b -> [a] -> PrettyP
interleave x xs = do
  env <- ask
  let x' = env `ppshow` x in
      tell $ intercalate x' $ map (ppshow env) xs

(<+>) :: (Pretty a, Pretty b) => a -> b -> PrettyP
a <+> b = pprint a >> pprint b
infixl 3 <+>

str :: Pretty a => a -> PrettyP
str = delim (greyOut "\"") (greyOut "\"") . strClr

ppr :: Pretty a => a -> IO ()
ppr = putStrLn . runPrinter defaults . pprint

instance Pretty PrettyP where pprint = id
instance Pretty String where pprint = tell
instance Pretty Char   where pprint = tell . (:[])
instance Pretty a => Pretty (ZipList a) where
  pprint (ZipList x) = interleave ", " x

instance (Pretty a, Pretty b) => Pretty (Map.Map a b) where
  pprint mp = do
    x <- ask
    braces $ intercalate "," $ map (\(k, v) -> x `ppshow` k ++ " => " ++ x `ppshow` v) $ Map.assocs mp

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pprint (a, b, s) = a <+> s <+> b

instance Pretty a => Pretty (a -> PrettyP, a) where
  pprint (x, y) = x y

instance Pretty Int
instance Pretty Double
instance Pretty Float
instance Pretty Integer
