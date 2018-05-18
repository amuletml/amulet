{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Parser.Wrapper
  ( Token(..)
  , AlexInput(..)
  , PState(..)
  , Parser
  , Action
  , alexInputPrevChar, alexGetByte
  , failWith
  , getStartCode, setStartCode
  , getInput, setInput
  , getState, setState, mapState
  , getPos
  , runParser, runLexer
  ) where

import Control.Monad.Fail as MonadFail
import Control.Monad.Writer

import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import Data.Word (Word8)
import Data.Int (Int64)
import Data.Char (ord)
import Data.Position
import Data.Spanned

import qualified Parser.Unicode as U
import Parser.Context
import Parser.Token
import Parser.Error

import Pretty


data AlexInput = LI { liPos  :: !SourcePos
                    , liText :: !L.Text
                    , liPrev :: !Char
                    , liIdx  :: !Int64 }

data PState = PState { stringBuffer :: B.Builder -- Builder for string literals
                     , commentDepth :: !Int      -- Depth for current file
                     , modulePrefix :: [T.Text]  -- List of module prefixes (in reversed order)
                     , tokenStart   :: !SourcePos -- The position of the start of this token

                     , context :: [Context]
                     , pending :: PendingState

                     , sPos  :: !SourcePos -- Current source position
                     , sText :: !L.Text    -- Current input
                     , sPrev :: !Char      -- Character before the input
                     , sIdx  :: !Int64     -- Offset into the whole input
                     , sMode :: !Int       -- Current startcode

                     , sErrors :: [ParseError]
                     }

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = liPrev

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte LI{ liPos = p, liText = t, liIdx = n } =
  case L.uncons t of
    Nothing -> Nothing
    Just (c, t') ->
      let b | c <= '\x7f' = fromIntegral (ord c)
            | otherwise = U.asFakeWord (U.classify c)
      in Just (b, LI { liPos = alexMove p c
                     , liText = t'
                     , liPrev = c
                     , liIdx = n + 1 })

alexMove :: SourcePos -> Char -> SourcePos
alexMove (SourcePos f l _) '\n' = SourcePos f (l + 1) 1
alexMove (SourcePos f l c) _     = SourcePos f l (c + 1)

data ParseResult a
  = POK PState a
  | PFailed [ParseError]

instance Show a => Show (ParseResult a) where
  show (POK _ s) = "(POK" ++ show s ++ ")"
  show (PFailed errs) = "(PFailed " ++ show errs ++ ")"

instance Pretty a => Pretty (ParseResult a) where
  pretty (POK _ s) = pretty s
  pretty (PFailed es) = vsep (map (\e -> pretty (annotation e) <> string ":" <+> pretty e) es)

newtype Parser a = P { unP :: PState -> ParseResult a }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = a `seq` (P $ flip POK a)
  (<*>) = ap

instance Monad Parser where
  (P m) >>= k = P $ \s -> case m s of
    POK s' a -> unP (k a) s'
    PFailed e -> PFailed e
  fail = MonadFail.fail

instance MonadFail Parser where
  fail msg = P $ \s -> PFailed (Failure (sPos s) msg:sErrors s)

instance MonadWriter [ParseError] Parser where
  tell es = P $ \s -> POK s { sErrors = es ++ sErrors s } ()
  pass m = P $ \s ->
    case unP m s of
      PFailed e -> PFailed e
      POK s' (a, f) -> POK s' { sErrors = f (sErrors s') } a
  listen m = P $ \s ->
    case unP m s of
      PFailed e -> PFailed e
      POK s' a -> POK s' (a, sErrors s')

failWith :: ParseError -> Parser a
failWith e = P $ \s -> PFailed (e:sErrors s)

getStartCode :: Parser Int
getStartCode = P $ \s -> POK s (sMode s)

setStartCode :: Int -> Parser ()
setStartCode m = P $ \s -> POK (s { sMode = m }) ()

getInput :: Parser AlexInput
getInput = P $ \s -> POK s LI { liPos  = sPos s
                              , liText = sText s
                              , liPrev = sPrev s
                              , liIdx  = sIdx  s }

setInput :: AlexInput -> Parser ()
setInput p = P $ \s -> POK (s { sPos  = liPos  p
                              , sText = liText p
                              , sPrev = liPrev p
                              , sIdx  = liIdx  p }) ()

getState :: Parser PState
getState = P $ \s -> POK s s

setState :: PState -> Parser ()
setState s = P $ \_ -> POK s ()

mapState :: (PState -> PState) -> Parser ()
mapState f = P (flip POK () . f)

getPos :: Parser SourcePos
getPos = P $ \s -> POK s (sPos s)

type Action a = AlexInput -> Int64 -> Parser a

runParser :: SourceName -> L.Text -> Parser a -> (Maybe a, [ParseError])
runParser file input m =
  let defaultState  = PState { stringBuffer = mempty
                             , commentDepth = 0
                             , modulePrefix = []
                             , tokenStart = SourcePos file 1 1

                             , context = defaultContext
                             , pending = Done

                             , sPos  = SourcePos file 1 1
                             , sText = input
                             , sPrev = '\n'
                             , sIdx  = 0
                             , sMode = 0

                             , sErrors = []
                             }
  in case unP m defaultState of
       PFailed err -> (Nothing, reverse err)
       POK s' a -> (Just a, reverse (sErrors s'))

runLexer :: SourceName -> L.Text -> Parser Token -> (Maybe [Token], [ParseError])
runLexer file input m = runParser file input gather where
  gather = do
    t <- m
    case t of
      Token TcEOF _ -> return [t]
      _   -> (t:) <$> gather
