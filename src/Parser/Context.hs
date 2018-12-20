{-# LANGUAGE FlexibleContexts #-}

{-| In order to determine how indentation should be handled, we keep track
  of the current context we're in. For instance, if we're in a @let@
  context then a dedent will insert a virtual @in@ token.

  Note that the contexts are pushed and popped just like they would be in
  the parser.
-}
module Parser.Context
  ( Context
  , PendingState(..)
  , defaultContext
  , handleContext
  ) where

import Control.Monad
import Control.Monad.Writer

import Data.Position
import Data.Spanned
import Data.Span

import Parser.Error
import Parser.Token

-- | An element in the context stack.
data Context
  -- | @(@, @[@ and @{@ (closed by @)@, @]@ and @}@ respectively).
  = CtxBracket TokenClass
  -- | A top-level let definition
  | CtxStmtLet SourcePos
  -- | An expression level let definition
  | CtxLet SourcePos
  -- | A list of sequences. The bool represents whether a separator is
  -- needed
  | CtxEmptyBlock (Maybe TokenClass)
  | CtxBlock SourcePos Bool (Maybe TokenClass)

  -- | The terms between a @match@ and a @with@ token.
  | CtxMatch SourcePos
  -- | An empty @match@ or @function@ expression
  | CtxMatchEmptyArms
  -- | A @match@ or @function@ expression with at least one arm. The
  -- position marks the location of the first arm.
  | CtxMatchArms SourcePos

  -- | The arguments of a @fun@. Namely, everything between @fun@ and
  -- @->@.
  | CtxFun SourcePos

  -- | The if context spans the entire if block. then spans from @then@
  -- to the @else@ keword.
  | CtxIf SourcePos | CtxThen SourcePos
  -- | else unresolved marks an else block which has not yet seen a
  -- token. This is used in order to merge `else if`.
  | CtxElseUnresolved SourcePos | CtxElse SourcePos

  -- | Anything between a module and the =, then the actual contents of
  -- the module
  | CtxModuleHead SourcePos
  | CtxModuleBodyUnresolved SourcePos SourcePos
  | CtxModuleBody SourcePos

  -- | The head of a type definition and its body
  | CtxTypeHead SourcePos | CtxTypeBody SourcePos

  -- | The head of a class definition and its body
  | CtxClassHead SourcePos | CtxClassBody SourcePos

  -- | The head of an instance and its body
  | CtxInstHead SourcePos | CtxInstBody SourcePos

  -- | The body of a list expression. This will be inside a @CtxBracket@,
  -- so is only used as a marker.
  | CtxList
  -- | The body of a list comprehension. This will be inside a
  -- @CtxBracket@, so is only used as a marker.
  | CtxListComprehension

  deriving (Show, Eq)

-- | Represents a "working list" of tokens the context processor is
-- currently handling.
data PendingState
  -- | There are no more tokens left to emit.
  = Done
  -- | This token requires more processing
  | Working Token
  -- | Push a new token to the parser, with additional tokens which may
  -- need processing.
  | Result Token PendingState   deriving (Show)

-- | The starting context for a set of top-level statements
defaultContext :: [Context]
defaultContext = [CtxBlock (SourcePos "" 0 1) False Nothing]

-- | Track our current context stack. This consumes a token and builds up
-- the context stack, returning any additional tokens which should be
-- emitted.
handleContext :: (Applicative f, MonadWriter (f ParseError) m)
              => Token -> [Context]
              -> m (PendingState, [Context])
handleContext = handleContextBlock True

-- Handles the indentation sensitive parts of the context tracker
handleContextBlock :: (Applicative f, MonadWriter (f ParseError) m)
                   => Bool -> Token -> [Context]
                   -> m (PendingState, [Context])
handleContextBlock needsSep  tok@(Token tk tp te) c =
  case (tk, c) of
    -- If we've got an empty block then we need to define the first position
    (_, CtxEmptyBlock end:cks) -> handleContext tok (CtxBlock tp False end:cks)

    -- If we've got the end of stream, then pop contexts and push the
    -- appropriate ending tokens.
    (TcEOF, ck:cks) ->
      case insertFor ck of
        Nothing -> handleContext tok cks
        Just x -> pure (Result (Token x te te) (Working tok), cks)

    -- If this token may pop some parent context, then pop
    -- as many contexts as possible
    (_, ck:cks)
      | case tk of
          TcTopSep -> not (terminates tk c)
          _ -> canTerminate tk && not (terminates tk c) && multiAny (terminates tk) cks
      -> case insertFor ck of
          Nothing -> handleContext tok cks
          Just x -> pure (Result (Token x tp te) (Working tok), cks)

    -- If we've got an in, then pop our let context and push a block
    (TcIn, CtxLet offside:cks) -> do
      -- If we're on the same line then it can be anywhere. Otherwise the
      -- in should line up with the let.
      when (spLine tp /= spLine offside && spCol tp /= spCol offside) $
        tell . pure $ UnalignedIn (annotation tok) (mkSpan1 offside)

      pure ( Result tok Done
           , CtxEmptyBlock (Just TcVEnd):cks )

    (_, CtxBracket tk':cks) | tk == tk' -> pure (Result tok Done, cks)

    -- Offside rule for blocks
    (_, CtxBlock offside _ end:cks)
      | spCol tp < spCol offside
      -> case end of
           Nothing -> handleContext tok cks
           Just x -> pure (Result (Token x tp te) (Working tok), cks)

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
    -- @
    --   foo
    --   |> bar
    -- @
    (_, CtxBlock offside True end:cks)
      | needsSep
      , spCol tp == spCol offside && spLine tp /= spLine offside
      , not (isOp tk)
      -> pure ( Result (Token TcVSep tp te) (Working tok)
              , CtxBlock offside False end:cks)

    -- Offside rule for statement lets: just pop the context
    (_, CtxStmtLet offside:cks)
      | (if tk == TcAnd then spCol tp + 1 else spCol tp) <= spCol offside
      -> handleContext tok cks
    -- Offside rule for expression lets: push an in and replace the context with a new block
    (_, CtxLet offside:cks)
      | (if tk == TcAnd then spCol tp + 1 else spCol tp) <= spCol offside
      -> pure ( Result (Token TcVIn tp te) $ Working tok
              , CtxEmptyBlock (Just TcVEnd):cks )

    -- Offside rule for matches. Pipes must be aligned in order to be
    -- considered acceptable.  Other characters which are not inside the
    -- block also pop the context This allows for something like
    --
    --   match x with
    --   | _ -> 1
    --   f x
    (_, CtxMatchArms offside:ck)
      | case tk of
          TcPipe -> spCol tp < spCol offside
          _ -> spCol tp <= spCol offside
      -> pure (Result (Token TcVEnd tp te) (Working tok), ck)

    -- Offside rule for ifs
    (_, CtxIf offside:ck)
      | (if isIfContinue tk then spCol tp + 1 else spCol tp) <= spCol offside
      -> pure ( Result (Token TcVEnd tp te) (Working tok)
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
              , CtxEmptyBlock Nothing : CtxModuleBody mod : ck )
    -- Otherwise assume it's an implicit begin
    (_, CtxModuleBodyUnresolved mod eq:ck)
      -> pure (Result (Token TcVBegin eq eq) (Working tok)
              , CtxEmptyBlock (Just TcVEnd) : CtxModuleBody mod : ck)

    -- Offside rule for class blocks
    (TcEnd, CtxClassBody offside:ck)
      | spCol tp == spCol offside
      -> pure (Result tok Done, ck)
    (_, CtxClassBody offside:ck)
      | spCol tp <= spCol offside
      -> handleContext tok ck
    -- For empty classes, we need to add @begin@ and @end@
    (_, CtxClassHead offside:ck)
      | (if tk == TcBegin then spCol tp + 1 else spCol tp) <= spCol offside
      -> pure (Result (Token TcVBegin tp tp) (Result (Token TcVEnd tp tp) (Working tok))
              , ck)

    -- We need to determine if we need to insert a begin or not for classes
    (TcBegin, CtxClassHead cls:ck)
      | spCol tp >= spCol cls
      -> pure ( Result tok Done
              , CtxEmptyBlock Nothing : CtxClassBody cls : ck )
    -- If it's part of the body, add an implicit begin. Note that we explicitly
    -- check its a val, so we don't handle multi-line class names.
    (TcVal, CtxClassHead cls:ck)
      | spCol tp > spCol cls
      -> pure (Result (Token TcVBegin tp tp) (Working tok)
              , CtxEmptyBlock (Just TcVEnd) : CtxClassBody cls : ck)

    -- Offside rule for instance blocks
    (TcEnd, CtxInstBody offside:ck)
      | spCol tp == spCol offside
      -> pure (Result tok Done, ck)
    (_, CtxInstBody offside:ck)
      | spCol tp <= spCol offside
      -> handleContext tok ck
    -- For empty instances, we need to add @begin@ and @end@
    (_, CtxInstHead offside:ck)
      | (if tk == TcBegin then spCol tp + 1 else spCol tp) <= spCol offside
      -> pure (Result (Token TcVBegin tp tp) (Result (Token TcVEnd tp tp) (Working tok))
              , ck )

    -- We need to determine if we need to insert a begin or not for instance blocks
    (TcBegin, CtxInstHead cls:ck)
      | spCol tp >= spCol cls
      -> pure ( Result tok Done
              , CtxEmptyBlock Nothing : CtxInstBody cls : ck )
    -- If it's part of the body, add an implicit begin. Note that we explicitly
    -- check its a @let@, so we don't handle multi-line class names.
    (TcLet, CtxInstHead cls:ck)
      | spCol tp > spCol cls
      -> pure (Result (Token TcVBegin tp tp) (Working tok)
              , CtxEmptyBlock (Just TcVEnd) : CtxInstBody cls: ck)

    -- Offside rule for type declarations. We allow the pipe to be aligned to
    -- the current context.
    (_, CtxTypeBody offside:ck)
      | (if tk == TcPipe then spCol tp + 1 else spCol tp) <= spCol offside
      -> handleContext tok ck
    -- @type ... =@ ~~> Replace type head with type body context
    (TcEqual, CtxTypeHead offside:ck)
      | spCol tp >= spCol offside
      -> pure (Result tok Done, CtxTypeBody offside:ck)
    -- Offside rule for type headers
    (_, CtxTypeHead offside:ck)
      | spCol tp <= spCol offside -> handleContext tok ck

    -- @let ...@ ~~> Push a let context
    (TcLet, _) -> pure
      ( Result tok Done
      , ( if isToplevel c || isListComprehension c
          then CtxStmtLet tp
          else CtxLet tp ):c )
    -- @let ...@ = ~~> Push a block context
    (TcEqual, CtxStmtLet _:_) -> pure
      ( Result tok Done
      , CtxEmptyBlock (Just TcVEnd):c )
    (TcEqual, CtxLet _:_) -> pure
      ( Result tok Done
      , CtxEmptyBlock (Just TcVEnd):c )

    -- @function@ ~~> Push a function context
    (TcFunction, _) -> pure (Result tok Done, CtxMatchEmptyArms:c)
    -- match ~~> Push a match context
    (TcMatch, _) -> pure (Result tok Done, CtxMatch tp:c)
    -- @match ... with~ ~~> Replace match with a match arm context
    (TcWith, CtxMatch _:ck) -> pure (Result tok Done, CtxMatchEmptyArms:ck)

    -- @function |@
    -- @match ... with |@ ~~>
    -- ~~> Define the first position of our match body
    (TcPipe, CtxMatchEmptyArms:ck) -> handleContext tok (CtxMatchArms tp:ck)
    --- @function ()@
    --- @match ... with ()@ ~~>
    (TcOParen, CtxMatchEmptyArms:ck) -> handleContext tok ck

    -- @| ... ->@ ~~> Push a new begin context
    (TcArrow, CtxMatchArms _:_) -> pure
      ( Result tok Done
      , CtxEmptyBlock Nothing:c )

    -- @fun@ ~~> Push a fun context
    (TcFun, _) -> pure (Result tok Done, CtxFun tp:c)
    -- @fun ... ->@ ~~> Pop function and push block
    (TcArrow, CtxFun _:ck) -> pure
      ( Result tok Done
      , CtxEmptyBlock (Just TcVEnd):ck )

    -- @if@ ~~> Push an if context
    (TcIf, _) -> pure (Result tok Done, CtxIf tp:c)
    -- @if@ ~~> Push a then context and a block
    (TcThen, _) -> pure
      ( Result tok Done
      , CtxEmptyBlock Nothing : CtxThen tp:c)
    (TcElse, _) -> pure
      ( Result tok Done
      , CtxElseUnresolved tp : c )

    -- @module@ ~~> Push a module context
    (TcModule, _) -> pure (Result tok Done, CtxModuleHead tp:c)
    (TcEqual, CtxModuleHead mod:ck) -> pure
      ( Result tok Done
      , CtxModuleBodyUnresolved mod te:ck)

    -- @class@ ~~> Push a class context
    (TcClass, _) -> pure (Result tok Done, CtxClassHead tp:c)
    -- @instance@ ~~> Push an instance context
    (TcInstance, _) -> pure (Result tok Done, CtxInstHead tp:c)
    -- @type@ ~~> Push a type context
    (TcType, _) -> pure (Result tok Done, CtxTypeHead tp:c)

    -- @begin ...@ ~~> CtxEmptyBlock : CtxBracket(end)
    (TcBegin, _) -> pure
      ( Result tok Done
      , CtxEmptyBlock Nothing:CtxBracket TcEnd:c)
    -- @(@, @{@, @[@  ~~> CtxBracket()|}|])
    (TcOParen, _) -> pure (Result tok Done, CtxBracket TcCParen:c)
    (TcOBrace, _) -> pure (Result tok Done, CtxBracket TcCBrace:c)
    (TcOSquare, _) -> pure (Result tok Done, CtxList:CtxBracket TcCSquare:c)

    -- @[ ... | @ ~~> CtxListComprehension
    (TcPipe, CtxList:ck) -> pure (Result tok Done, CtxListComprehension:ck)

    _ -> pure (Result tok Done, c)

-- | If this token can be considered an operator. Namely, if it can be
-- aligned on the current offside line.
isOp :: TokenClass -> Bool
isOp TcDot = True
isOp TcComma = True
isOp TcColon = True
isOp TcOp{} = True
isOp TcOpIdent{} = True
isOp TcOpIdentQual{} = True
isOp _ = False

-- | What token must be inserted to close this context
insertFor :: Context -> Maybe TokenClass
insertFor (CtxBlock _ _ t) = t
insertFor CtxLet{} = Just TcVIn
insertFor CtxStmtLet{} = Nothing
insertFor CtxMatchArms{} = Just TcVEnd
insertFor CtxIf{} = Just TcVEnd
insertFor _ = Nothing

-- | If this token may terminate some context.
--
-- This is just an optimisation so we don't need to check 'terminates'
-- for every token.
canTerminate :: TokenClass -> Bool
canTerminate TcCBrace = True
canTerminate TcCParen = True
canTerminate TcCSquare = True
canTerminate TcComma = True
canTerminate TcElse = True
canTerminate TcEnd = True
canTerminate TcIn = True
canTerminate TcSemicolon = True
canTerminate TcTopSep = True
canTerminate TcAnd = True
canTerminate _ = False

-- | If this 'TokenClass' may terminate the current context.
--
-- Note, this should /not/ check the parent contexts. That will be done
-- within the actual context checker.
terminates :: TokenClass -> [Context] -> Bool

-- `in` terminates the `let` binding
terminates TcIn (CtxLet{}:_) = True
terminates TcCSquare (CtxLet{}:_) = True
terminates TcComma (CtxLet{}:_) = True

-- `and` terminates the `let` binding
terminates TcAnd (CtxLet{}:_) = True
terminates TcAnd (CtxStmtLet{}:_) = True

-- `else` terminates the `then` context, so we want to pop everything until the
-- nearest `if`.
terminates TcElse (CtxIf{}:_) = True

-- Any bracket is terminated by the closing bracket
terminates t (CtxBracket t':_) | t == t' = True

-- When observing a comma, we want to close everything up to the nearest bracket
-- pair.
terminates TcComma (CtxBracket TcCParen:_) = True
terminates TcComma (CtxBracket TcCBrace:_) = True
terminates TcComma (CtxBracket TcCSquare:_) = True

-- Toplevel terminators
terminates TcEnd (CtxModuleBody{}:_) = True
terminates TcEnd (CtxClassBody{}:_) = True
terminates TcEnd (CtxInstBody{}:_) = True

-- Block level terminators
terminates TcTopSep (CtxBlock{}:ck) = isToplevel ck
terminates TcSemicolon (CtxBlock{}:ck) = not (isToplevel ck)

-- List comprehension statement terminator
terminates TcComma (CtxListComprehension:_) = True

terminates _ _ = False

-- | Is this context's immediate parent the top-level?
isToplevel :: [Context] -> Bool
isToplevel [] = True
isToplevel (CtxModuleBody{}:_) = True
isToplevel (CtxClassBody{}:_) = True
isToplevel (CtxInstBody{}:_) = True
isToplevel (CtxTypeHead{}:_) = True
isToplevel (CtxTypeBody{}:_) = True
isToplevel (CtxBlock{}:cks) = isToplevel cks
isToplevel _ = False

-- | Is this context's immediate parent a list comprehension?
isListComprehension :: [Context] -> Bool
isListComprehension (CtxListComprehension:_) = True
isListComprehension _ = False

isIfContinue :: TokenClass -> Bool
isIfContinue TcThen = True
isIfContinue TcElse = True
isIfContinue _ = False

-- | Determines if any tail of the list matches the predicate.
multiAny :: ([a] -> Bool) -> [a] -> Bool
multiAny _ [] = False
multiAny f x@(_:xs) = f x || multiAny f xs
