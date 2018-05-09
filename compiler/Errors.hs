{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-import-lists #-}
module Errors where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Foldable
import Data.Position
import Data.Spanned
import Data.Span

import Control.Applicative
import Control.Arrow (second)

import qualified "amuletml" Control.Monad.Infer as I

import qualified "amuletml" Syntax.Resolve as R

import "amuletml" Pretty

type FileMap = [(SourceName, T.Text)]

report :: Pretty p => p -> FileMap -> IO ()
report err _ = putDoc (pretty err)

reportI :: I.TypeError -> FileMap -> IO ()
reportI err files
  | (err', Just (reason, loc)) <- innermostError err
  = reportP (I.ArisingFrom err' reason) loc files
  where
    innermostError (I.ArisingFrom err p) = second (<|> Just (p, annotation p)) $ innermostError err
    innermostError err = (err, Nothing)
reportI err _ = putDoc (pretty err)

reportR :: R.ResolveError -> FileMap -> IO ()
reportR err files
  | Just (err', loc) <- innermostError err
  = reportP err' loc files
  where
    innermostError e@(R.ArisingFrom err p) = innermostError err <|> Just (e, annotation p)
    innermostError e@(R.ArisingFromTop err p) = innermostError err <|> Just (e, annotation p)
    innermostError _ = Nothing
reportR err _ = putDoc (pretty err)

reportS :: (Pretty a, Spanned a) => a -> FileMap -> IO ()
reportS err = reportP err (annotation err)

reportP :: Pretty a => a -> Span -> FileMap -> IO ()
reportP err loc files =
  let SourcePos{ spLine = startLine, spCol = start } = spanStart loc
      SourcePos{ spLine = endLine, spCol = end } = spanEnd loc
      file = maybe T.empty snd (find ((==fileName loc) . fst) files)
      lines = drop (startLine - 1) (T.lines file)
      linePad = length (show endLine) + 1
      putLine before body = T.putStrLn
                            $ T.pack "\x1b[1;34m"
                            <> T.justifyRight linePad ' ' before <> spaceC <> pipeC
                            <> T.pack "\x1b[0m"
                            <> spaceC <> body
   in do
     putDoc (pretty err)

     putLine T.empty T.empty
     traverse_ (uncurry (putLine . T.pack . show)) (zip [startLine..] (take (endLine - startLine + 1) lines))
     if startLine == endLine
         then putLine T.empty (T.replicate (start - 1) spaceC <> T.replicate (end - start + 1) underC)
         else putLine T.empty T.empty
  where
    pipeC = T.singleton '│'
    underC = T.singleton '~'
    spaceC = T.singleton ' '
