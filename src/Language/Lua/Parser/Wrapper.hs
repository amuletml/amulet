{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

{-| An alternative wrapper for the Alex lexer and Happy parser. This
  operates on 'T.Text' objects instead of Alex's default bytestrings.

  As the lexer and parser work in sync (we only consume one token at a
  time), we must manage the state of both at the same time.
-}
module Language.Lua.Parser.Wrapper
  ( SourcePos(..)
  , Token(..)
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
  , runParser
  ) where

import Control.Monad.Fail as MonadFail
import Control.Monad.Writer

import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as L

import Data.Word (Word8)
import Data.Int (Int64)
import Data.Char (ord)
import Data.Position
import Data.Spanned

import Text.Pretty.Semantic

import Language.Lua.Parser.Token
import Language.Lua.Parser.Error

-- | The current Alex input, for consumption by the lexer
data AlexInput = LI { liPos  :: !SourcePos
                    , liText :: !L.Text
                    , liPrev :: !Char
                    , liIdx  :: !Int64 }

-- | The current state of the lexer and parser
data PState = PState { stringBuffer :: B.Builder  -- ^ Builder for string literals
                     , stringChar   :: Char       -- ^ The starting character for the string
                     , tokenStart   :: !SourcePos -- ^ The position of the start of this token
                     , trivials     :: Bool       -- ^ Whether "trivial" tokens such as whitespace and comments should be emitted.

                     , sPos  :: !SourcePos -- ^ Current source position
                     , sText :: !L.Text    -- ^ Current input
                     , sPrev :: !Char      -- ^ Character before the input
                     , sIdx  :: !Int64     -- ^ Offset into the whole input
                     , sMode :: !Int       -- ^ Current startcode
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
            | otherwise = 0 :: Word8
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
  -- | Parsing failed with an error.
  | PFailed ParseError

instance Show a => Show (ParseResult a) where
  show (POK _ s) = "(POK" ++ show s ++ ")"
  show (PFailed err) = "(PFailed " ++ show err ++ ")"

instance Pretty a => Pretty (ParseResult a) where
  pretty (POK _ s) = pretty s
  pretty (PFailed e) = pretty (annotation e) <> string ":" <+> pretty e

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
  fail msg = P $ \s -> PFailed (Failure (sPos s) msg)

-- | Abort the parse with an error
failWith :: ParseError -> Parser a
failWith e = P $ \_ -> PFailed e

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
runParser :: SourcePos -> L.Text -> Parser a -> Either ParseError a
runParser = runParser' False

runParser' :: Bool -> SourcePos -> L.Text -> Parser a -> Either ParseError a
runParser' trivial pos input m =
  let defaultState  = PState { stringBuffer = mempty
                             , stringChar   = '\0'
                             , tokenStart   = pos
                             , trivials     = trivial

                             , sPos  = pos
                             , sText = input
                             , sPrev = '\n'
                             , sIdx  = 0
                             , sMode = 0
                             }
  in case unP m defaultState of
       PFailed err -> Left err
       POK _ a -> Right a
