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
import "amuletml" Control.Monad.Infer

import qualified "amuletml" Syntax.Resolve as R

import "amuletml" Pretty

report :: Pretty p => p -> T.Text -> IO ()
report err _ = putDoc (pretty err)

reportI :: I.TypeError -> T.Text -> IO ()
reportI err file
  | (err', Just (reason, loc)) <- innermostError err
  = reportP (I.ArisingFrom err' reason) loc file
  where
    innermostError (I.ArisingFrom err p) = second (<|> Just (BecauseOf p, annotation p)) $ innermostError err
    innermostError err = (err, Nothing)
reportI err _ = putDoc (pretty err)

reportR :: R.ResolveError -> T.Text -> IO ()
reportR err file
  | Just (err', loc) <- innermostError err
  = reportP err' loc file
  where
    innermostError e@(R.ArisingFrom err p) = innermostError err <|> Just (e, annotation p)
    innermostError e@(R.ArisingFromTop err p) = innermostError err <|> Just (e, annotation p)
    innermostError _ = Nothing
reportR err _ = putDoc (pretty err)

reportP :: Pretty a => a -> Span -> T.Text -> IO ()
reportP err loc file =
  let SourcePos{ spLine = startLine, spCol = start } = spanStart loc
      SourcePos{ spLine = endLine, spCol = end } = spanEnd loc
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
