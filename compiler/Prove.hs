{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts #-}
module Main where

import System.Console.Haskeline hiding (display)
import System.Environment

import Control.Monad.IO.Class
import Control.Monad.Namey
import Control.Lens

import Parser.Wrapper (runParser)
import Parser (parseType, getL)

import qualified Data.Text.Lazy as L
import Data.Text.Lazy (Text)
import Data.Foldable
import Data.List

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Text.Pretty.Note (Note(..))
import Text.Pretty.Semantic

import Syntax.Transform
import Syntax.Builtin
import Syntax.Subst
import Syntax.Types
import Syntax

import qualified Syntax.Resolve.Scope as R
import Syntax.Resolve
import Syntax.Desugar

import Types.Infer
import Types.Holes

import Errors

import Data.Span
import Data.These

main :: IO ()
main = do
  x <- getArgs
  case x of
    [] -> putDoc ("Welcome to" <+> keyword "amc-prove") *> runInputT defaultSettings prover
    _ -> putDoc proverHelp

prover :: InputT IO ()
prover = do
  sentence <- getInputLine "? "
  case sentence of
    Nothing -> pure ()
    Just x | ":q" `isPrefixOf` x -> pure ()
    Just x | ":h" `isPrefixOf` x -> liftIO (putDoc proverHelp) *> prover
    Just t -> handleSentence (L.pack t)

handleSentence :: Text -> InputT IO ()
handleSentence sentence =
  case runParser "=stdin" sentence parseType of
    (Just t, _) -> flip evalNameyT (TgName "a" 0) $ proveSentence (`reportS` files) (propVarToTv (getL t))
    (Nothing, es) -> liftIO $ traverse_ (`reportS` files) es
  *> prover
  where files = [("<input>", L.toStrict sentence)]

propVarToTv :: Type Parsed -> Type Parsed
propVarToTv = transformType go where
  go (TyPromotedCon v) = TyVar v
  go x = x

proveSentence :: (forall a. Note a Style => a -> IO ())
              -> Type Parsed -> NameyT (InputT IO) ()
proveSentence report t = do
  let prog = [ TySymDecl Public (Name "_") [] (foldr addForall t (ftv t)) internal ]
      addForall v = TyForall v (Just TyType)
  x <- resolveProgram rScope builtinModules prog
  case x of
    Left es -> liftIO $ traverse_ report es
    Right (p, _) -> do
      x <- inferProgram env =<< desugarProgram p
      case x of
        This es -> liftIO $ traverse_ report es
        These _ (p, _) -> solve p
        That (p, _) -> solve p
  where
    solve [ TypeDecl _ _ _ (Just [ArgCon _ _ ty _]) _ ] = do
      candidates <- findHoleCandidate mempty internal env ty
      if not (null candidates)
         then liftIO $ putDoc (keyword "yes." <#> indent 2 (pretty (head candidates)))
         else liftIO $ putDoc (keyword "probably not.")
    solve _ = undefined

rScope :: R.Scope
rScope = builtinResolve { R.tyScope = addBuiltins (R.tyScope builtinResolve) } where
  addBuiltins x = foldr (\v m -> Map.insert (Name v) (R.SVar (TgInternal v)) m) x builtins
  builtins =
    Set.fromList [ "+", "not", "ff", "tt", "<->" ]

env :: Env
env =
  builtinEnv & types %~ mappend tys
             & names %~ focus (teleFromList bindings)
             & constructors %~ mappend cons
  where
    cons = Set.fromList [TgInternal "L", TgInternal "R", TgInternal "Not", TgInternal "T"]
    tys = Map.fromList [ (TgInternal "+", Set.fromList [TgInternal "L", TgInternal "R"])
                       , (TgInternal "not", Set.fromList [TgInternal "Not"])
                       , (TgInternal "ff", mempty)
                       , (TgInternal "tt", Set.fromList [TgInternal "T"])
                       , (TgInternal "<->", Set.fromList [TgInternal "Equiv"])
                       ]

    bindings = [ (TgInternal "+", TyType :-> TyType :-> TyType) 
               , ( TgInternal "L"
                 , TyForall a (Just TyType) $
                   TyForall b (Just TyType) $
                     TyVar a :-> TyOperator (TyVar a) (TgInternal "+") (TyVar b)
                 )
               , ( TgInternal "R"
                 , TyForall a (Just TyType) $
                   TyForall b (Just TyType) $
                     TyVar b :-> TyOperator (TyVar a) (TgInternal "+") (TyVar b)
                 )
               , ( TgInternal "<->", TyType :-> TyType :-> TyType )
               , ( TgInternal "Equiv"
                 , TyForall a (Just TyType) $
                   TyForall b (Just TyType) $
                     TyTuple (TyVar a :-> TyVar b)
                             (TyVar b :-> TyVar a)
                       :-> TyOperator (TyVar a) (TgInternal "<->") (TyVar b)
                 )
               , ( TgInternal "not", TyType :-> TyType )
               , ( TgInternal "Not"
                 , TyForall a (Just TyType) $
                   (TyVar a :-> TyForall b (Just TyType) (TyVar b)) :-> TyApps (TyCon (TgInternal "not")) [TyVar a]
                 )
               , (TgInternal "ff", TyType :-> TyType)
               , (TgInternal "tt", TyType :-> TyType)
               , ( TgInternal "T", TyCon (TgInternal "tt") )
               ]
    a = TgInternal "a"
    b = TgInternal "b"

proverHelp :: Doc
proverHelp = vsep
  [ "A prover for" <+> keyword "constructive quantifier-free first-order logic" <+> "based on amc's hole-filling."
  , empty
  , indent 2 . align $ 
      vsep [ bullet $ "Write propositional variables in" <+> stypeCon "UPPERCASE"
           , bullet $ "The logical connectives ∧ (AND) and ∨ (OR) are written infix as * and +"
              <#> indent 4 (hsep [ stypeCon "P" <+> soperator "+" <+> stypeCon "Q"
                                 , comma
                                 , stypeCon "P" <+> soperator "*" <+> stypeCon "Q"
                                 ])
           , bullet $ "The values ⊤ (TRUE) and ⊥ (FALSE) are spelled" <+> sliteral "tt" <+> "and" <+> sliteral "ff"
           , bullet $ "Negation is the function" <+> skeyword "not"
           , bullet $ "Additionally, propositional bi-implication may be written"
              <+> hsep [ stypeCon "P" <+> soperator "<->" <+> stypeCon "Q" ]
           ]
  , empty
  , keyword "Note:" <+> "amc-prove has difficulties dealing with uses of" <+> parens (soperator "+") <+> "inside" <+> parens (soperator "*")
  , indent 6 "please use multiple assumptions instead."
  , indent 6 "(Try"
      <+> hsep [ stypeCon "P" <+> arrow <+> stypeCon "Q" <+> arrow <+> stypeCon "R" ]
      <+> "instead of"
      <+> hsep [ stypeCon "P" <+> soperator "*" <+> stypeCon "Q" <+> arrow <+> stypeCon "R" ]
      <> ".)"
  ]
