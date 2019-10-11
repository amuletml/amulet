{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}

{-| Various utilities for reporting errors to the user. -}
module Frontend.Errors
  ( ErrorBundle(..)
  , parseErrors, resolveErrors, typeErrors, verifyErrors
  , report, hReport
  , reportAll, hReportAll, reportAllS
  ) where

import Control.Monad.IO.Class
import Control.Lens

import System.IO

import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Position

import Control.Monad.Infer (TypeError)
import Syntax.Resolve (ResolveError)
import Parser.Error (ParseError)
import Syntax.Verify (VerifyError)

import qualified Text.Pretty.Note as N
import qualified Text.Pretty.Ansi as A
import qualified Text.Pretty as P
import Text.Pretty.Semantic

import Parser.Wrapper
import Parser.Lexer
import Parser.Token

-- | A bundle of all possible errors which may be encountered while compiling a
-- file.
data ErrorBundle = Errors
  { _parseErrors   :: [ParseError]
  , _resolveErrors :: [ResolveError]
  , _typeErrors    :: [TypeError]
  , _verifyErrors  :: [VerifyError]
  }

makeLenses ''ErrorBundle

instance Semigroup ErrorBundle where
  (Errors p r t v) <> (Errors p' r' t' v') = Errors (p <> p') (r <> r') (t <> t') (v <> v')

instance Monoid ErrorBundle where
  mempty = Errors mempty mempty mempty mempty

-- | Report a note, converting it into a simple document suitable for rendering.
report :: N.Note a Style => N.FileMap -> a -> SimpleDoc (Either N.NoteStyle Style)
report fs
  = filterSimpleDoc (either (const True) uncommentFilter)
  . renderPretty 0.4 100
  . N.format (N.fileSpans fs highlightAmulet)

-- | Report all messages in a bundle.
reportAll :: N.FileMap -> ErrorBundle -> SimpleDoc (Either N.NoteStyle Style)
reportAll fs (Errors ps rs ts vs)
  = filterSimpleDoc (either (const True) uncommentFilter)
  . renderPretty 0.4 100
  . foldr (<##>) mempty
  $ fmt ps <> fmt rs <> fmt ts <> fmt vs
  where
    fmt :: N.Note a Style => [a] -> [P.Doc (Either N.NoteStyle Style)]
    fmt = map (N.format (N.fileSpans fs highlightAmulet))

-- | Report a note to a handle - be it a terminal or file.
hReport :: (MonadIO m, N.Note a Style) => Handle -> N.FileMap -> a -> m ()
hReport h fs = displayDoc h . report fs

hReportAll :: MonadIO m => Handle -> N.FileMap -> ErrorBundle -> m ()
hReportAll h fs  = displayDoc h . reportAll fs

reportAllS :: MonadIO m => N.FileMap -> ErrorBundle -> m ()
reportAllS fs  = displayDoc stdout . reportAll fs

displayDoc :: MonadIO m => Handle -> SimpleDoc (Either N.NoteStyle Style) -> m ()
displayDoc h err = liftIO $ do
  term <- hIsTerminalDevice h
  let display = if term then A.displayDecorated else P.display
      displayed = display (either N.toAnsi toAnsi <$> err)
  if T.null displayed
     then pure ()
     else T.hPutStrLn h displayed

highlightAmulet :: T.Text -> N.Highlighted Style
highlightAmulet t =
  case runLexer "" (L.fromStrict t) lexerScan of
    (Nothing, _) -> N.defaultHighlight t
    (Just ts, _) -> buildTokens 0 ts

  where
    buildTokens p [] = [(text, T.drop p t)]
    buildTokens p (Token kind s e:ts) =
      let s' = spCol s - 1
          e' = spCol e
      in (text, T.take (s' - p) . T.drop p $ t)
       : (fmap Right . toStyle kind . text, T.take (e' - s') . T.drop s' $ t)
       : buildTokens e' ts

    toStyle TcIdentifier{} = id
    toStyle TcOpIdent{} = soperator
    toStyle TcConIdent{} = stypeCon

    toStyle TcIdentifierQual{} = id
    toStyle TcOpIdentQual{} = soperator
    toStyle TcConIdentQual{} = stypeCon

    toStyle TcDotQual{} = id
    toStyle TcOp{} = soperator
    toStyle TcTyvar{} = stypeVar
    toStyle TcAccess{} = id
    toStyle TcHole{} = id

    toStyle TcString{} = sstring
    toStyle TcFloat{} = sliteral
    toStyle TcInteger{} = sliteral

    toStyle TcComment{} = scomment
    toStyle TcWhitespace{} = id

    toStyle t
      | t >= TcArrow && t <= TcUnderscore = soperator
      | t >= TcComma && t <= TcCBanana = soperator
      | t >= TcLet && t <= TcAs = soperator
      | otherwise = id
