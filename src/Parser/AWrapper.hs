{-# OPTIONS_GHC -funbox-strict-fields #-}

module Parser.AWrapper
  ( SourceName
  , SourcePos(..)
  , Token(..)
  , AlexInput(..)
  , PState(..)
  , ParseResult(..)
  , Parser
  , Action
  , alexInputPrevChar, alexGetByte
  , failPos, failSpan
  , getStartCode, setStartCode
  , getInput, setInput
  , getState, setState
  , getPos
  , runParser
  ) where

import Control.Monad
import Control.Monad.Fail as MonadFail

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Internal as B (w2c)
import qualified Data.ByteString.Lazy as B
import Data.Span
import Data.Int (Int64)
import Data.Spanned
import Data.Word (Word8)

import Text.Parsec.Pos (newPos)

import Parser.Token

import Pretty

type SourceName = String

data SourcePos = SourcePos { spFile :: SourceName
                           , spLine :: !Int
                           , spCol  :: !Int }
  deriving Show

data Token = Token !TokenClass !SourcePos deriving Show

mkSpan1' :: SourcePos -> Span
mkSpan1' (SourcePos f l c) = mkSpan1 (newPos f l c)

instance Spanned Token where
  annotation (Token _ s) = mkSpan1' s

data AlexInput = LI { liPos  :: !SourcePos
                    , liText :: !B.ByteString
                    , liPrev :: !Char }

data PState = PState { stringBuffer :: B.Builder -- Builder for string literals
                     , commentDepth :: Int -- Depth for current file

                     , sPos  :: !SourcePos   -- Current source position
                     , sText :: B.ByteString -- Current input
                     , sPrev :: !Char        -- Character before the input
                     , sMode :: !Int        -- Current startcode
                     }

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar LI{ liPrev = c } = c

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte LI{ liPos = p, liText = t } =
    case B.uncons t of
        Nothing -> Nothing
        Just (b, t') ->
            let c = B.w2c b
                p'  = alexMove p c
            in Just (b, LI { liPos = p'
                           , liText = t'
                           , liPrev = c })

alexMove :: SourcePos -> Char -> SourcePos
alexMove (SourcePos f l _) '\n' = SourcePos f (l + 1) 1
alexMove (SourcePos f l c) _     = SourcePos f l (c + 1)

data ParseResult a
  = POK PState a
  | PFailed String Span

instance Show a => Show (ParseResult a) where
  show (POK _ s) = show s
  show (PFailed msg pos) = show pos ++ ": " ++ msg

instance Pretty a => Pretty (ParseResult a) where
  pprint (POK _ s) = pprint s
  pprint (PFailed msg pos) = pos <+> ": " <+> msg

newtype Parser a = P { unP :: PState -> ParseResult a }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = a `seq` (P $ flip POK a)
  (<*>) = ap

instance Monad Parser where
  (P m) >>= k = P $ \s -> case m s of
                            POK s' a -> unP (k a) s'
                            PFailed err pos -> PFailed err pos
  fail = MonadFail.fail

instance MonadFail Parser where
  fail msg = P $ \s -> PFailed msg (mkSpan1' (sPos s))

failPos :: String -> SourcePos -> Parser a
failPos msg p = P $ \_ -> PFailed msg (mkSpan1' p)

failSpan :: String -> Span -> Parser a
failSpan msg p = P $ \_ -> PFailed msg p

getStartCode :: Parser Int
getStartCode = P $ \s -> POK s (sMode s)

setStartCode :: Int -> Parser ()
setStartCode m = P $ \s -> POK (s { sMode = m }) ()

getInput :: Parser AlexInput
getInput = P $ \s -> POK s LI { liPos  = sPos s
                              , liText = sText s
                              , liPrev = sPrev s }

setInput :: AlexInput -> Parser ()
setInput p = P $ \s -> POK (s { sPos  = liPos p
                              , sText = liText p
                              , sPrev = liPrev p }) ()

getState :: Parser PState
getState = P $ \s -> POK s s

setState :: PState -> Parser ()
setState s = P $ \_ -> POK s ()

getPos :: Parser SourcePos
getPos = P $ \s -> POK s (sPos s)

type Action a = AlexInput -> Int64 -> Parser a

runParser :: SourceName -> B.ByteString -> Parser a -> ParseResult a
runParser file input m = unP m PState { stringBuffer = mempty
                                      , commentDepth = 0

                                      , sPos  = SourcePos file 1 1
                                      , sText = input
                                      , sPrev = '\n'
                                      , sMode = 0 }
