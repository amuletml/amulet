{-# LANGUAGE FlexibleContexts #-}
module Parser.Context
  ( Context
  , PendingState(..)
  , defaultContext
  , handleContext
  ) where

import Control.Monad
import Control.Monad.Report

import Data.Position

import Parser.Error
import Parser.Token

{-
  In order to determine how indentation should be handled, we keep track
  of the current context we're in. For instance, if we're in a let
  context then a dedent will insert a virtual `in` token.

  Note that the contexts are pushed and popped just like they would be in
  the parser.

-}

data Context
  -- -- (, [ and { (closed by ), ] and } respectively).
  = CtxBracket TokenClass
  -- A top-level let definition
  | CtxStmtLet SourcePos
  -- An expression level let definition
  | CtxLet SourcePos
  -- A list of sequences. The bool represents whether a separator is needed
  | CtxEmptyBlock (Maybe TokenClass)
  | CtxBlock SourcePos Bool (Maybe TokenClass)

  | CtxMatch SourcePos
  | CtxMatchEmptyArms
  | CtxMatchArms SourcePos

  | CtxFun SourcePos

  -- The if context spans the entire if block. then spans from `then` to the
  -- `else` keword.
  | CtxIf SourcePos | CtxThen SourcePos
  -- else unresolved marks an else block which has not yet seen a token. This is
  -- used in order to merge `else if`.
  | CtxElseUnresolved SourcePos | CtxElse SourcePos

  -- Anything between a module and the =, then the actual contents of the module
  | CtxModuleHead SourcePos
  | CtxModuleBodyUnresolved SourcePos SourcePos
  | CtxModuleBody SourcePos

  deriving (Show, Eq)

data PendingState
  = Done
  | Working Token
  | Result Token PendingState
  deriving (Show)

defaultContext :: [Context]
defaultContext = [CtxBlock (SourcePos "" 0 1) False Nothing]

handleContext :: MonadReport ParseError m
              => Token -> [Context]
              -> m (PendingState, [Context])
handleContext = handleContextBlock True

-- Handles the indentation sensitive parts of the context tracker
handleContextBlock :: MonadReport ParseError m
                   => Bool -> Token -> [Context]
                   -> m (PendingState, [Context])
handleContextBlock needsSep  tok@(Token tk tp) c =
  case (tk, c) of
    -- If we've got an empty block then we need to define the first position
    (_, CtxEmptyBlock end:cks) -> handleContext tok (CtxBlock tp False end:cks)

    -- If we've got the end of stream, then pop contexts and push the
    -- appropriate ending tokens.
    (TcEOF, ck:cks) ->
      case insertFor ck of
        Nothing -> handleContext tok cks
        Just x -> pure (Result (Token x tp) (Working tok), cks)

    -- If this token may pop some parent context, then pop
    -- as many contexts as possible
    (_, ck:cks)
      | case tk of
          TcTopSep -> not (terminates tk c)
          _ -> canTerminate tk && not (terminates tk c) && multiAny (terminates tk) cks
      -> case insertFor ck of
          Nothing -> handleContext tok cks
          Just x -> pure (Result (Token x tp) (Working tok), cks)

    -- If we've got an in, then pop our let context and push a block
    -- TODO: Consider where this rule should occur, or warn if the indentation is funky.
    (TcIn, CtxLet offside:cks) -> do
      when (spCol tp < spCol offside) (report (UnindentIn tp offside))

      pure ( Result tok Done
           , CtxEmptyBlock (Just TcVEnd):cks )
    -- TODO: Pop other contexts if this occurs
    (_, CtxBracket tk':cks) | tk == tk' -> pure (Result tok Done, cks)

    -- Offside rule for blocks
    (_, CtxBlock offside _ end:cks)
      | spCol tp < spCol offside
      -> case end of
           Nothing -> handleContext tok cks
           Just x -> pure (Result (Token x tp) (Working tok), cks)

    -- If we're inside a context which doesn't need a separator,
    -- then convert it into one which does.
    (_, CtxBlock offside False end:cks) ->
      handleContextBlock False tok (CtxBlock offside True end:cks)

    -- Explicitly allow `;`/`;;` inside blocks
    (_, CtxBlock offside True end:cks)
      | needsSep
      , tk == TcTopSep && isToplevel cks
      -> pure ( Result tok Done
              , CtxBlock offside False end:cks)
      | needsSep
      , tk == TcSemicolon && not (isToplevel cks)
      -> pure ( Result tok Done
              , CtxBlock offside False end:cks)

    -- Offside rule for blocks, for tokens which are aligned with the current
    -- context and are not operators
    -- This allows for expressions like
    --
    -- foo
    -- |> bar
    (_, CtxBlock offside True end:cks)
      | needsSep
      , spCol tp == spCol offside && spLine tp /= spLine offside
      , not (isOp tk)
      -> pure ( Result (Token TcVSep tp) (Working tok)
              , CtxBlock offside False end:cks)

    -- Offside rule for statement lets: just pop the context
    (_, CtxStmtLet offside:cks)
      | (if tk == TcAnd then spCol tp + 1 else spCol tp) <= spCol offside
      -> handleContext tok cks
    -- Offside rule for expression lets: push an in and replace the context with a new block
    (_, CtxLet offside:cks)
      | (if tk == TcAnd then spCol tp + 1 else spCol tp) <= spCol offside
      -> pure ( Result (Token TcVIn tp) $ Working tok
              , CtxEmptyBlock (Just TcVEnd):cks )

    -- Offside rule for ifs
    (_, CtxIf offside:ck)
      | (if isIfContinue tk then spCol tp + 1 else spCol tp) <= spCol offside
      -> pure ( Result (Token TcVEnd tp) (Working tok)
              , ck )
    (_, CtxThen offside:ck)
      | spCol tp <= spCol offside
      -> handleContext tok ck
    (_, CtxElse offside:ck)
      | spCol tp <= spCol offside
      -> handleContext tok ck

    -- We process `else if` as a single token.
    (TcIf, CtxElseUnresolved offside:ck) | spLine tp == spLine offside
      -> pure ( Result tok Done
              , CtxEmptyBlock Nothing : CtxIf offside:ck )
    (_, CtxElseUnresolved _:ck)
      -> handleContext tok
           (CtxEmptyBlock Nothing : CtxElse tp:ck)

    -- Offside rule for modules
    (TcEnd, CtxModuleBody offside:ck)
      | spCol tp == spCol offside
      -> pure (Result tok Done, ck)
    (_, CtxModuleBody offside:ck)
      | spCol tp <= spCol offside
      -> handleContext tok ck

    -- We need to determine if we need to insert a begin or not
    -- If we're followed by a constructor, assume it's a module import
    (TcConIdent{}, CtxModuleBodyUnresolved{}:ck) -> pure (Result tok Done, ck)
    (TcConIdentQual{}, CtxModuleBodyUnresolved{}:ck) -> pure (Result tok Done, ck)
    -- Handle explicit begins
    (TcBegin, CtxModuleBodyUnresolved mod eq:ck)
      | spLine tp == spLine eq || spCol tp == spCol mod
      -> pure ( Result tok Done
              , CtxEmptyBlock Nothing : CtxModuleBody mod:ck )
    -- Otherwise assume it's an implicit begin
    (_, CtxModuleBodyUnresolved mod eq:ck)
      -> pure (Result (Token TcVBegin eq) (Working tok)
              , CtxEmptyBlock (Just TcVEnd) : CtxModuleBody mod :ck)

    -- let ... ~~> Push an let context
    (TcLet, _) -> pure
      ( Result tok Done
      , ( if isToplevel c
          then CtxStmtLet tp
          else CtxLet tp ):c )
    -- let ... = ~~> Push a $begin token and a block context
    (TcEqual, CtxStmtLet _:_) -> pure
      ( Result tok $ Result (Token TcVBegin tp) Done
      , CtxEmptyBlock (Just TcVEnd):c )
    (TcEqual, CtxLet _:_) -> pure 
      ( Result tok $ Result (Token TcVBegin tp) Done
      , CtxEmptyBlock (Just TcVEnd):c )

    -- function ~~> Push a function context
    (TcFunction, _) -> pure (Result tok Done, CtxMatchEmptyArms:c)
    -- match ~~> Push a match context
    (TcMatch, _) -> pure (Result tok Done, CtxMatch tp:c)
    -- match ... with ~~> Replace match with a match arm context
    (TcWith, CtxMatch _:ck) -> pure (Result tok Done, CtxMatchEmptyArms:ck)

    -- function |
    -- match... with | ~~>
    -- ~~> Define the first position of our match body
    (TcPipe, CtxMatchEmptyArms:ck) -> handleContext tok (CtxMatchArms tp:ck)

    -- Pipes must be aligned in order to be considered acceptable.
    -- Other characters which are not inside the block also pop the context
    -- This allows for something like
    --
    --   match x with
    --   | _ -> 1
    --   f x
    (_, CtxMatchArms offside:ck)
      | case tk of
          TcPipe -> spCol tp < spCol offside
          _ -> spCol tp <= spCol offside
      -> pure (Result (Token TcVEnd tp) (Working tok), ck)

    -- | ... -> ~~> Push a new begin context
    (TcArrow, CtxMatchArms _:_) -> pure
      ( Result tok Done
      , CtxEmptyBlock Nothing:c )

    -- fun ~~> Push a fun context
    (TcFun, _) -> pure (Result tok Done, CtxFun tp:c)
    -- fun ... -> ~~> Pop function and push block
    (TcArrow, CtxFun _:ck) -> pure
      ( Result tok Done
      , CtxEmptyBlock (Just TcVEnd):ck )

    -- if ~~> Push a if context
    (TcIf, _) -> pure (Result tok Done, CtxIf tp:c)
    -- if ~~> Push a then context and a block
    (TcThen, _) -> pure
      ( Result tok Done
      , CtxEmptyBlock Nothing : CtxThen tp:c)
    (TcElse, _) -> pure
      ( Result tok Done
      , CtxElseUnresolved tp :
        case c of
          CtxThen{}:ck -> ck
          ck -> ck )

    (TcModule, _) -> pure (Result tok Done, CtxModuleHead tp:c)
    (TcEqual, CtxModuleHead mod:ck) -> pure
      ( Result tok Done
      , CtxModuleBodyUnresolved mod tp:ck)

    -- begin ... ~~> CtxEmptyBlock : CtxBracket(end)
    (TcBegin, _) -> pure
      ( Result tok Done
      , CtxEmptyBlock Nothing:CtxBracket TcEnd:c)
    -- (, {, [   ~~> CtxBracket()|}|])
    (TcOParen, _) -> pure (Result tok Done, CtxBracket TcCParen:c)
    (TcOBrace, _) -> pure (Result tok Done, CtxBracket TcCBrace:c)
    (TcOSquare, _) -> pure (Result tok Done, CtxBracket TcCSquare:c)

    _ -> pure (Result tok Done, c)

isOp :: TokenClass -> Bool
isOp TcDot = True
isOp TcComma = True
isOp TcColon = True
isOp TcOp{} = True
isOp TcOpIdent{} = True
isOp TcOpIdentQual{} = True
isOp _ = False

insertFor :: Context -> Maybe TokenClass
insertFor (CtxBlock _ _ t) = t
insertFor CtxLet{} = Just TcVIn
insertFor CtxStmtLet{} = Nothing
insertFor CtxMatchArms{} = Just TcVEnd
insertFor CtxIf{} = Just TcVEnd
insertFor _ = Nothing

canTerminate :: TokenClass -> Bool
canTerminate TcIn = True
canTerminate TcCParen = True
canTerminate TcCBrace = True
canTerminate TcCSquare = True
canTerminate TcEnd = True
canTerminate TcElse = True
canTerminate _ = False

terminates :: TokenClass -> [Context] -> Bool
-- Some expression terminators
terminates TcIn (CtxLet{}:_) = True
terminates TcElse (CtxThen{}:_) = True
terminates t (CtxBracket t':_)  = t == t'
-- Toplevel terminators
terminates TcEnd (CtxModuleBody{}:_) = True
-- Block level terminators
terminates TcTopSep (CtxBlock{}:ck) = isToplevel ck
terminates TcSemicolon (CtxBlock{}:ck) = not (isToplevel ck)

terminates _ _ = False

isToplevel :: [Context] -> Bool
isToplevel [] = True
isToplevel (CtxModuleBody{}:_) = True
isToplevel (CtxBlock{}:cks) = isToplevel cks
isToplevel _ = False

isIfContinue :: TokenClass -> Bool
isIfContinue TcThen = True
isIfContinue TcElse = True
isIfContinue _ = False

multiAny :: ([a] -> Bool) -> [a] -> Bool
multiAny _ [] = False
multiAny f x@(_:xs) = f x || multiAny f xs
