{-# LANGUAGE FlexibleContexts, NamedFieldPuns, TemplateHaskell #-}

{-| Various utilities for reporting errors to the user. -}
module Frontend.Errors
  ( ErrorBundle(..)
  , parseErrors, resolveErrors, typeErrors, verifyErrors
  , ErrorFilter(..), defaultFilter, hasErrors
  , report, hReport
  , reportAll, hReportAll, reportAllS
  ) where

import Control.Monad.IO.Class
import Control.Lens

import System.IO

import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Position
import Data.Maybe

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
  deriving Show

makeLenses ''ErrorBundle

instance Semigroup ErrorBundle where
  (Errors p r t v) <> (Errors p' r' t' v') = Errors (p <> p') (r <> r') (t <> t') (v <> v')

instance Monoid ErrorBundle where
  mempty = Errors mempty mempty mempty mempty

-- | A filter to determine which notes/warnings should be promoted to an error
data ErrorFilter = ErrorFilter
  { filterAll :: Bool -- ^ Whether to promote all errors by default
  , filterInclude :: Set.Set Int -- ^ Promoted errors
  , filterExclude :: Set.Set Int -- ^ Demoted errors
  }
  deriving Show

defaultFilter :: ErrorFilter
defaultFilter = ErrorFilter False mempty mempty

hasErrors :: ErrorFilter -> ErrorBundle -> Bool
hasErrors (ErrorFilter all include exclude) es
   = go (es ^. parseErrors)
  || go (es ^. resolveErrors)
  || go (es ^. typeErrors)
  || go (es ^. verifyErrors)
  where
    go :: N.Note a s => [a] -> Bool
    go = any (\n -> N.diagnosticKind n > N.ErrorMessage || maybe False matches (N.noteId n))

    matches n
      | Set.member n include = True
      | Set.member n exclude = False
      | otherwise = all

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
  . vsep
  $ fmt ps <> fmt rs <> fmt ts <> fmt vs <> info
  where
    fmt :: N.Note a Style => [a] -> [P.Doc (Either N.NoteStyle Style)]
    fmt = map (N.format (N.fileSpans fs highlightAmulet))
    info :: [ P.Doc (Either N.NoteStyle Style) ]
    info =
      let errs = Set.toList (Set.fromList (mapMaybe N.noteId ps
                                        <> mapMaybe N.noteId rs
                                        <> mapMaybe N.noteId ts
                                        <> mapMaybe N.noteId vs))
          (them, errors, explanations) =
            case errs of
              [_] -> (string "it", string "message", string "has a detailed explanation")
              _ -> (string "them", string "messages", string "have detailed explanations")
          nums = hsep (punctuate comma (map int errs))
       in
        case errs of
          [] -> []
          _ ->
            [ empty
            , string "The following" <+> errors <+> explanations <> char ':' <+> nums <> char '.'
            ,     string "Try"
              <+> squote <> (Right <$> skeyword (string "amc explain" <+> int (head errs))) <> squote
              <+> string "to see" <+> them <> char '.'
            ]

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
  case runLexer mempty (L.fromStrict t) lexerScan of
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
      | t >= TcAnd && t <= TcWith = soperator
      | otherwise = id
