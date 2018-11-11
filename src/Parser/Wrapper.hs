{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-| An alternative wrapper for the Alex lexer and Happy parser. This
  operates on 'T.Text' objects instead of Alex's default bytestrings.

  As the lexer and parser work in sync (we only consume one token at a
  time), we must manage the state of both at the same time.
-}
module Parser.Wrapper
  ( Token(..)
  , AlexInput(..)
  , PState(..)
  , Parser
  , Action
  , alexInputPrevChar, alexGetByte
  , failWith, failWiths
  , tellWarnings, tellErrors
  , getStartCode, setStartCode
  , getInput, setInput
  , getState, setState, mapState
  , getPos
  , runParser, runLexer, runLexerTrivial
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

import Text.Pretty.Semantic

-- | The current Alex input, for consumption by the lexer
data AlexInput = LI { liPos  :: !SourcePos
                    , liText :: !L.Text
                    , liPrev :: !Char
                    , liIdx  :: !Int64 }

-- | The current state of the lexer and parser
data PState = PState { stringBuffer :: B.Builder  -- ^ Builder for string literals
                     , commentDepth :: !Int       -- ^ Depth for current file
                     , modulePrefix :: [T.Text]   -- ^ List of module prefixes (in reversed order)
                     , tokenStart   :: !SourcePos -- ^ The position of the start of this token
                     , trivials     :: Bool
                       -- ^ Whether "trivial" tokens such as whitespace and comments should be emitted.

                     , context :: [Context]    -- ^ The current 'Context' stack
                     , pending :: PendingState -- ^ The tokens which are awaiting consumption or context handling

                     , sPos  :: !SourcePos -- ^ Current source position
                     , sText :: !L.Text    -- ^ Current input
                     , sPrev :: !Char      -- ^ Character before the input
                     , sIdx  :: !Int64     -- ^ Offset into the whole input
                     , sMode :: !Int       -- ^ Current startcode

                     , sErrors  :: [ParseError]
                       -- ^ List of emitted warnings, with the head being the most recent
                     , sErrored :: Bool         -- ^ Whether parsing failed somewhere
                     }

-- | Extract the last consumed character
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = liPrev

-- | Get the current byte of the input. This will classify unicode
-- characters using 'U.asFakeWord'.
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

-- | Consume one character from the current input
alexMove :: SourcePos -> Char -> SourcePos
alexMove (SourcePos f l _) '\n' = SourcePos f (l + 1) 1
alexMove (SourcePos f l c) _     = SourcePos f l (c + 1)

-- | Represents the result of a parse operation
data ParseResult a
  -- | A successful operation, meaning parsing can continue
  = POK PState a
  -- | Parsing failed. This contains a list of emitted warnings, with the
  -- head being the first one.
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

-- ^ Report one or more warnings within the parser
tellWarnings :: [ParseError] -> Parser ()
tellWarnings = tell

-- ^ Report one or more critical errors within the parser
tellErrors  :: [ParseError] -> Parser ()
tellErrors x = tell x >> mapState (\s -> s { sErrored = True })

-- | Abort the parse with an error
failWith :: ParseError -> Parser a
failWith e = P $ \s -> PFailed (e:sErrors s)

-- | Abort the parse with 1 or more errors.
failWiths :: [ParseError] -> Parser a
failWiths e = P $ \s -> PFailed (reverse e ++ sErrors s)

-- | Get the current start code for the lexer
getStartCode :: Parser Int
getStartCode = P $ \s -> POK s (sMode s)

-- | Set the start code for the lexer
setStartCode :: Int -> Parser ()
setStartCode m = P $ \s -> POK (s { sMode = m }) ()

-- | Get the current input for the lexer
getInput :: Parser AlexInput
getInput = P $ \s -> POK s LI { liPos  = sPos s
                              , liText = sText s
                              , liPrev = sPrev s
                              , liIdx  = sIdx  s }

-- | Set the current input for the lexer
setInput :: AlexInput -> Parser ()
setInput p = P $ \s -> POK (s { sPos  = liPos  p
                              , sText = liText p
                              , sPrev = liPrev p
                              , sIdx  = liIdx  p }) ()

-- | Get the current parser state
getState :: Parser PState
getState = P $ \s -> POK s s

-- | Set the current parser state
setState :: PState -> Parser ()
setState s = P $ \_ -> POK s ()

-- | Apply a function to the current parser state
mapState :: (PState -> PState) -> Parser ()
mapState f = P (flip POK () . f)

-- | Get the current position in the input text
getPos :: Parser SourcePos
getPos = P $ \s -> POK s (sPos s)

-- | An action performed by the lexer, which consumes the current input
-- and produces some object (normally a token).
type Action a = AlexInput -> Int64 -> SourcePos -> Parser a

-- | Run the parser monad, returning the result and a list of errors and
-- warnings
runParser :: SourceName -> L.Text -> Parser a -> (Maybe a, [ParseError])
runParser = runParser' False

runParser' :: Bool -> SourceName -> L.Text -> Parser a -> (Maybe a, [ParseError])
runParser' trivial file input m =
  let defaultState  = PState { stringBuffer = mempty
                             , commentDepth = 0
                             , modulePrefix = []
                             , tokenStart   = SourcePos file 1 1
                             , trivials     = trivial

                             , context = defaultContext
                             , pending = Done

                             , sPos  = SourcePos file 1 1
                             , sText = input
                             , sPrev = '\n'
                             , sIdx  = 0
                             , sMode = 0

                             , sErrors  = []
                             , sErrored = False
                             }
  in case unP m defaultState of
       PFailed err -> (Nothing, reverse err)
       POK s' a | sErrored s' -> (Nothing, reverse (sErrors s'))
                | otherwise -> (Just a, reverse (sErrors s'))

-- | Run the parser monad on a lexing function, returning a list of
-- tokens and all warnings/errors.
runLexer :: SourceName -> L.Text -> Parser Token -> (Maybe [Token], [ParseError])
runLexer = runLexer' False

-- | Run the parser monad on a lexing function, returning a list of
-- normal and trivial tokens as well all warnings/errors.
runLexerTrivial :: SourceName -> L.Text -> Parser Token -> (Maybe [Token], [ParseError])
runLexerTrivial = runLexer' True

runLexer' :: Bool -> SourceName -> L.Text -> Parser Token -> (Maybe [Token], [ParseError])
runLexer' trivial file input m = runParser' trivial file input gather where
  gather = do
    t <- m
    case t of
      Token TcEOF _ _ -> return [t]
      _   -> (t:) <$> gather
