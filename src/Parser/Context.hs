{-# LANGUAGE MultiWayIf #-}
module Parser.Context
  ( Context
  , PendingState(..)
  , defaultContext
  , handleContext
  ) where


import Data.Position
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

  deriving (Show, Eq)

data PendingState
  = Done
  | Working Token
  | Result Token PendingState
  deriving (Show)

defaultContext :: [Context]
defaultContext = [CtxBlock (SourcePos "" 0 1) False Nothing]

handleContext :: Token -> [Context] -> (PendingState, [Context])
handleContext = handleContextBlock True

-- Handles the indentation sensitive parts of the context tracker
handleContextBlock :: Bool -> Token -> [Context] -> (PendingState, [Context])
handleContextBlock needsSep  tok@(Token tk tp) c = -- traceShow =<< (\(t, c') -> (tk, t, c')) $
  case (tk, c) of
    -- If we've got an empty block then we need to define the first position
    (_, CtxEmptyBlock end:cks) -> handleContext tok (CtxBlock tp False end:cks)

    -- If we've got the end of stream, then pop contexts and push the
    -- appropriate ending tokens.
    (TcEOF, ck:cks) ->
      case insertFor ck of
        Nothing -> handleContext tok cks
        Just x -> (Result (Token x tp) (Working tok), cks)

    -- If this token may pop some parent context, then pop
    -- as many contexts as possible
    (_, ck:cks)
      | canTerminate tk
      , not (terminates tk ck)
      , any (terminates tk) cks
      -> case insertFor ck of
          Nothing -> handleContext tok cks
          Just x -> (Result (Token x tp) (Working tok), cks)

    -- If we've got an in, then pop our let context and push a block
    -- TODO: Consider where this rule should occur, or warn if the indentation is funky.
    (TcIn, CtxLet _:cks) -> ( Result tok Done, CtxEmptyBlock (Just TcVEnd):cks )
    -- TODO: Pop other contexts if this occurs
    (_, CtxBracket tk':cks) | tk == tk' -> (Result tok Done, cks)

    -- Offside rule for blocks
    (_, CtxBlock offside _ end:cks)
      | spCol tp < spCol offside
      -> case end of
           Nothing -> handleContext tok cks
           Just x -> (Result (Token x tp) (Working tok), cks)

    -- If we're inside a context which doesn't need a separator,
    -- then convert it into one which does.
    (_, CtxBlock offside False end:cks) ->
      handleContextBlock False tok (CtxBlock offside True end:cks)

    -- Explicitly allow `;`/`;;` inside blocks
    (_, CtxBlock offside True end:cks)
      | needsSep
      , tk == TcTopSep && isToplevel cks
      -> ( Result tok Done, CtxBlock offside False end:cks)
      | needsSep
      , tk == TcSemicolon && not (isToplevel cks)
      -> ( Result tok Done, CtxBlock offside False end:cks)

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
      -> ( Result (Token TcVSep tp) (Working tok)
         , CtxBlock offside False end:cks)

    -- Offside rule for statement lets: just pop the context
    (_, CtxStmtLet offside:cks)
      | tk == TcTopSep || (if tk == TcAnd then spCol tp + 1 else spCol tp) <= spCol offside
      -> handleContext tok cks
    -- Offside rule for expression lets: push an in and replace the context with a new block
    (_, CtxLet offside:cks)
      | (if tk == TcAnd then spCol tp + 1 else spCol tp) <= spCol offside
      -> ( Result (Token TcVIn tp) $ Working tok
         , CtxEmptyBlock (Just TcVEnd):cks )

    -- let ... ~~> Push an let context
    (TcLet, _) ->
      ( Result tok Done
      , ( if isToplevel c
          then CtxStmtLet tp
          else CtxLet tp ):c )
    -- let ... = ~~> Push a $begin token and a block context
    (TcEqual, CtxStmtLet _:_) ->
      ( Result tok $ Result (Token TcVBegin tp) Done
      , CtxEmptyBlock (Just TcVEnd):c )
    (TcEqual, CtxLet _:_) ->
      ( Result tok $ Result (Token TcVBegin tp) Done
      , CtxEmptyBlock (Just TcVEnd):c )

    -- function ~~> Push a function context
    (TcFunction, _) -> (Result tok Done, CtxMatchEmptyArms:c)
    -- match ~~> Push a match context
    (TcMatch, _) -> (Result tok Done, CtxMatch tp:c)
    -- match ... with ~~> Replace match with a match arm context
    (TcWith, CtxMatch _:ck) -> (Result tok Done, CtxMatchEmptyArms:ck)

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
      -> (Result (Token TcVEnd tp) (Working tok), ck)

    -- | ... -> ~~> Push a new begin context
    (TcArrow, CtxMatchArms _:_) ->
      ( Result tok (Result (Token TcVBegin tp) Done)
      , CtxEmptyBlock (Just TcVEnd):c )

    -- begin ... ~~> CtxEmptyBlock : CtxBracket(end)
    (TcBegin, _) -> (Result tok Done, CtxEmptyBlock Nothing:CtxBracket TcEnd:c)
    -- (, {, [   ~~> CtxBracket()|}|])
    (TcOParen, _) -> (Result tok Done, CtxBracket TcCParen:c)
    (TcOBrace, _) -> (Result tok Done, CtxBracket TcCBrace:c)
    (TcOSquare, _) -> (Result tok Done, CtxBracket TcCSquare:c)

    _ -> (Result tok Done, c)

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
insertFor _ = Nothing

canTerminate :: TokenClass -> Bool
canTerminate TcIn = True
canTerminate TcTopSep = True
canTerminate TcCParen = True
canTerminate TcCBrace = True
canTerminate TcCSquare = True
canTerminate TcEnd = True
canTerminate _ = False

terminates :: TokenClass -> Context -> Bool
terminates TcIn CtxLet{} = True
terminates TcTopSep CtxStmtLet{} = True
terminates t (CtxBracket t')  = t == t'
terminates _ _ = False

isToplevel :: [Context] -> Bool
isToplevel [] = True
isToplevel (CtxBlock{}:cks) = isToplevel cks
isToplevel _ = False
