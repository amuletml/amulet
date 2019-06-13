{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

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

import Text.Pretty.Semantic

import Parser.Error
import Parser.Token

-- | An element in the context stack.
data Context
  -- | Inside a matching pair of brackets (we store the required end
  -- bracket). Used for @begin@/@end@, @()@, @[]@, @{}@.
  = CtxBracket TokenClass

  -- | An empty sequence of terms. Used before pushing 'CtxBlock' when
  -- you do not know where the next token will occur.
  | CtxEmptyBlock (Maybe TokenClass)
  -- | A sequence of terms
  | CtxBlock { blockStart :: SourcePos -- ^ The position of the first token within this block
             , blockSep :: Bool -- ^ Whether this block is awaiting a separator.
             , blockEmpty :: Bool -- ^ Whether this block is empty. Namely, whether we've seen a separator yet.
             , blockTerm :: Maybe TokenClass -- ^ The terminator for this block.
             }

  -- | A top-level let definition.
  | CtxStmtLet SourcePos
  -- | An expression level let definition.
  | CtxLet SourcePos

  -- | The terms between a @match@ and a @with@ token.
  | CtxMatch SourcePos
  -- | An empty @match@ or @function@ expression.
  | CtxMatchEmptyArms
  -- | A @match@ or @function@ expression with at least one arm. The
  -- position marks the location of the first arm.
  | CtxMatchArms SourcePos

  -- | The arguments of a @fun@. Namely, everything between @fun@ and
  -- @->@.
  | CtxFun SourcePos

  -- | The if context spans the entire if block.
  | CtxIf SourcePos
  -- | Everything after a @then@ keyword but before a @else@.
  | CtxThen SourcePos
  -- | An @else@ block which has not yet seen a token. This is used in
  -- order to provide special handling for @else if@.
  | CtxElseUnresolved SourcePos
  -- | The body of an @else@ block.
  | CtxElse SourcePos

  -- | Anything between @module@ and the @=@
  | CtxModuleHead Bool SourcePos
  -- | After the @=@ of a module, but before observing the actual contents.
  | CtxModuleBodyUnresolved SourcePos
  -- | The actual definition of a module. This always has a 'CtxBlock'
  -- inside it.
  | CtxModuleBody

  -- | The precursor to a type definition: everything between @type@ and
  -- the @=@.
  | CtxTypeHead SourcePos
  -- | The actual body of a type definition.
  | CtxTypeBody SourcePos

  -- | The head of a class definition, including signature.
  | CtxClassHead SourcePos
  -- | The body of a class definition.
  | CtxClassBody

  -- | The head of an instance definition.
  | CtxInstHead SourcePos
  -- | The body of an instance definition.
  | CtxInstBody

  -- | The body of a list expression.
  --
  -- This will be inside a 'CtxBracket', so is only used as a marker.
  | CtxList

  -- | The body of a list comprehension.
  --
  -- This will be inside a 'CtxBracket', so is only used as a marker.
  | CtxListComprehension

  -- | The body of a monad computation
  --
  -- This has very similar semantics to 'CtxListComprehension', in which
  -- lets are treated as statements instead.
  | CtxMonad

  deriving (Show, Eq)


source :: SourcePos -> Doc
source (SourcePos _ l c) = shown l <> colon <> shown c

instance Pretty Context where
  pretty (CtxBracket b) = soperator . string . show $ b

  pretty (CtxEmptyBlock Nothing) = stypeCon "EmptyBlock"
  pretty (CtxEmptyBlock (Just x)) = stypeCon "EmptyBlock" <> brackets (shown x)
  pretty (CtxBlock start sep empty Nothing) = stypeCon "Block" <> brackets ("sep=" <> shown sep <> ",empty=" <> shown empty) <> parens (source start)
  pretty (CtxBlock start sep empty (Just x)) = stypeCon "Block" <> brackets (shown x <> ",sep=" <> shown sep <> ",empty=" <> shown empty) <> parens (source start)

  pretty (CtxStmtLet sp) = stypeCon "StmtLet" <> parens (source sp)
  pretty (CtxLet sp) = stypeCon "Let" <> parens (source sp)
  pretty (CtxMatch sp) = stypeCon "Match" <> parens (source sp)
  pretty CtxMatchEmptyArms = stypeCon "MatchEmptyArms"
  pretty (CtxMatchArms sp) = stypeCon "MatchArms" <> parens (source sp)
  pretty (CtxFun sp) = stypeCon "Fun" <> parens (source sp)
  pretty (CtxIf sp) = stypeCon "If" <> parens (source sp)
  pretty (CtxThen sp) = stypeCon "Then" <> parens (source sp)
  pretty (CtxElse sp) = stypeCon "Else" <> parens (source sp)
  pretty (CtxElseUnresolved sp) = stypeCon "ElseUnresolved" <> parens (source sp)

  pretty (CtxModuleHead False sp) = stypeCon "ModuleHead" <> parens (source sp)
  pretty (CtxModuleHead True sp) = stypeCon "ModuleHead" <> brackets "Top" <> parens (source sp)
  pretty (CtxModuleBodyUnresolved sp) = stypeCon "ModuleUnresolved" <> parens (source sp)
  pretty CtxModuleBody = stypeCon "Module"

  pretty CtxList = stypeCon "List"
  pretty CtxListComprehension = "ListComprehension"
  pretty CtxMonad = stypeCon "Monad"

  pretty x = parens . string . show $ x

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

infixr `Result`

-- | The starting context for a set of top-level statements
defaultContext :: [Context]
defaultContext = [CtxBlock (SourcePos "" 0 1) False True Nothing]

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
    (_, CtxEmptyBlock end:cks) -> handleContext tok (CtxBlock tp False True end:cks)

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

    (_, CtxBracket tk':cks) | tk == tk' -> pure (Result tok Done, cks)

    -- Offside rule for blocks
    (_, CtxBlock offside _ _ end:cks)
      | spCol tp < spCol offside
      -> case end of
           Nothing -> handleContext tok cks
           Just x -> pure (Result (Token x tp te) (Working tok), cks)

    -- If we're inside a context which doesn't need a separator,
    -- then convert it into one which does.
    (_, CtxBlock offside False first end:cks) ->
      handleContextBlock False tok (CtxBlock offside True first end:cks)

    -- Explicitly allow `;`/`;;` inside blocks
    (_, CtxBlock offside True _ end:cks)
      | needsSep
      , tk == TcTopSep && isToplevel cks
      -> pure ( Result tok Done
              , CtxBlock offside False False end:cks)
      | needsSep
      , tk == TcSemicolon && not (isToplevel cks)
      -> pure ( Result tok Done
              , CtxBlock offside False False end:cks)

    -- Offside rule for blocks, for tokens which are aligned with the current
    -- context and are not operators
    -- This allows for expressions like
    --
    -- @
    --   foo
    --   |> bar
    -- @
    (_, CtxBlock offside True _ end:cks)
      | needsSep
      , spCol tp == spCol offside && spLine tp /= spLine offside
      , not (isOp tk)
      -> pure ( Result (Token TcVSep tp te) (Working tok)
              , CtxBlock offside False False end:cks)

    -- If we've got an in, then pop our let context and push a block
    (TcIn, CtxLet offside:cks) -> do
      -- If we're on the same line then it can be anywhere. Otherwise the
      -- in should line up with the let.
      when (spLine tp /= spLine offside && spCol tp /= spCol offside) $
        tell . pure $ UnalignedIn (annotation tok) (mkSpan1 offside)

      pure ( Result tok Done
           , CtxEmptyBlock (Just TcVEnd):cks )

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

    -- Offside rule for leading modules.
    --
    -- If we see a token aligned with a leading module declaration, we
    -- inject an @= $begin@ and switch to a module body.
    (_, CtxModuleHead True mod:ck)
      | spCol tp <= spCol mod
      -> pure ( Token TcEqual tp tp `Result` Token TcVBegin tp tp `Result` Working tok
              , CtxEmptyBlock (Just TcVEnd) : CtxModuleBody : ck )

    -- Offside rule for modules
    (_, CtxModuleBody:ck) -> handleContext tok ck

    -- @module ... = begin@ ~~> Push module + bracket
    (TcBegin, CtxModuleBodyUnresolved mod:ck)
      | spCol tp >= spCol mod
      -> pure ( Result tok Done
              , CtxEmptyBlock Nothing : CtxModuleBody : CtxBracket TcEnd : ck )
    -- @module ... = ?toplevel@ ~~> Add implicit begin and enter module.
    (tc, CtxModuleBodyUnresolved mod:ck)
      | spCol tp > spCol mod
      , isTopTok tc
      -> pure ( Token TcVBegin tp tp `Result` Working tok
              , CtxEmptyBlock (Just TcVEnd) : CtxModuleBody : ck )
    (_, CtxModuleBodyUnresolved _:ck)
      -> handleContext tok ck
    -- @module ... =@ ~~> Push a unresolved module body
    (TcEqual, CtxModuleHead _ mod:ck)
      -> pure ( Result tok Done
              , CtxModuleBodyUnresolved mod : ck)

    -- Offside rule for class blocks
    (_, CtxClassBody:ck) -> handleContext tok ck

    -- @class ... begin@ ~~> Push empty block + class + bracket
    (TcBegin, CtxClassHead cls:ck)
      | spCol tp >= spCol cls
      -> pure ( Result tok Done
              , CtxEmptyBlock Nothing : CtxClassBody : CtxBracket TcEnd : ck )
    -- @class ... ?toplevel@ ~~> Add implicit begin and enter class
    (tc, CtxClassHead cls:ck)
      | spCol tp > spCol cls
      , isTopTok tc
      -> pure ( Token TcVBegin tp tp `Result` Working tok
              , CtxEmptyBlock (Just TcVEnd) : CtxClassBody : ck )
    -- Offside rule for empty classes
    (_, CtxClassHead offside:ck)
      | spCol tp <= spCol offside
      -> pure ( Token TcVBegin tp tp `Result` Token TcVEnd tp tp `Result` Working tok
              , ck)

    -- Offside rule for instance blocks
    (_, CtxInstBody:ck) -> handleContext tok ck

    -- @instance ... begin@ ~~> Push empty block + instance + bracket
    (TcBegin, CtxInstHead cls:ck)
      | spCol tp >= spCol cls
      -> pure ( Result tok Done
              , CtxEmptyBlock Nothing : CtxInstBody : CtxBracket TcEnd : ck )
    -- @instance ... ?toplevel@ ~~> Add implicit begin and enter instance
    (tc, CtxInstHead cls:ck)
      | spCol tp > spCol cls
      , isTopTok tc
      -> pure ( Token TcVBegin tp tp `Result` Working tok
              , CtxEmptyBlock (Just TcVEnd) : CtxInstBody : ck )
    -- Offside rule for empty instancees
    (_, CtxInstHead offside:ck)
      | spCol tp <= spCol offside
      -> pure ( Token TcVBegin tp tp `Result` Token TcVEnd tp tp `Result` Working tok
              , ck)

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
      , ( if isToplevel c || isStatement c
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

    -- @module@ at top of file ~~> Push a module context which doesn't require an equal
    (TcModule, [CtxBlock{ blockEmpty = True }]) -> pure (Result tok Done, CtxModuleHead True tp:c)

    -- @module@ ~~> Push a module context
    (TcModule, _) -> pure (Result tok Done, CtxModuleHead False (getMarginAt tp c):c)
    -- @class@ ~~> Push a class context
    (TcClass, _) -> pure (Result tok Done, CtxClassHead (getMarginAt tp c):c)
    -- @instance@ ~~> Push an instance context
    (TcInstance, _) -> pure (Result tok Done, CtxInstHead (getMarginAt tp c):c)
    -- @type@ ~~> Push a type context
    (TcType, _) -> pure (Result tok Done, CtxTypeHead tp:c)

    -- @begin ...@ ~~> CtxEmptyBlock : CtxBracket(end)
    (TcBegin, _) -> pure
      ( Result tok Done
      , CtxEmptyBlock Nothing:CtxMonad:CtxBracket TcEnd:c)
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
insertFor CtxBlock { blockTerm = t } = t
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
terminates TcTopSep (CtxBlock{}:ck) | isToplevel ck = True
terminates TcSemicolon (CtxBlock{}:ck) | not (isToplevel ck) = True
terminates TcSemicolon (CtxBlock{}:CtxMonad:_) = True

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

-- | Is this context's immediate parent "statement"-esque (@with@ and
-- statement-style @let@s)?
isStatement :: [Context] -> Bool
isStatement (CtxListComprehension:_) = True
isStatement (CtxBlock{}:CtxMonad:_) = True
isStatement _ = False

isIfContinue :: TokenClass -> Bool
isIfContinue TcThen = True
isIfContinue TcElse = True
isIfContinue _ = False

-- | This token starts a top-level term
isTopTok :: TokenClass -> Bool
isTopTok TcClass = True
isTopTok TcInstance = True
isTopTok TcLet = True
isTopTok TcModule = True
isTopTok TcType = True
isTopTok TcVal = True
isTopTok TcPrivate = True
isTopTok TcOpen = True
isTopTok _ = False

-- | Get the left most margin of the current context
getMargin :: [Context] -> SourcePos
getMargin (CtxBlock s _ _ _:_) = s
getMargin (_:xs) = getMargin xs
getMargin _ = error "Underflow when getting left margin"

-- | Get the margin at a particular line.
getMarginAt :: SourcePos -> [Context] -> SourcePos
getMarginAt pos c = (getMargin c) { spLine = spLine pos }

-- | Determines if any tail of the list matches the predicate.
multiAny :: ([a] -> Bool) -> [a] -> Bool
multiAny _ [] = False
multiAny f x@(_:xs) = f x || multiAny f xs
