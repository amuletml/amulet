module Amc.Repl.State
  ( ReplConfig(..)
  , ReplState(..)
  , defaultState
  , resetState
  ) where

import System.IO
import qualified Foreign.Lua.Core.Types as L
import qualified Foreign.Lua as L

import qualified Control.Monad.Infer as T

import qualified Data.Text as T

import qualified Backend.Lua.Emit as B

import qualified Frontend.Driver as D

import qualified Core.Lower as L

import qualified Syntax.Var as S
import qualified Syntax.Builtin as Bi
import Syntax.Resolve.Scope (Signature(..))

import Amc.Debug

data ReplConfig = ReplConfig
  { port         :: Int
  , debugMode    :: DebugMode
  , root         :: FilePath
  , driverConfig :: D.DriverConfig
  , prelude      :: Maybe FilePath
  , coreLint     :: Bool
  }
  deriving Show

data ReplState = ReplState
  { resolveScope :: Signature
  , inferScope   :: T.Env
  , emitState    :: B.TopEmitState
  , lastName     :: S.Name
  , lowerState   :: L.LowerState

  , driver       :: D.Driver
  , config       :: ReplConfig

  , luaState     :: L.State

  , currentFile  :: Maybe FilePath
  , outputHandle :: Handle
  }

defaultState :: ReplConfig -> IO ReplState
defaultState config = do
  state <- L.newstate
  -- Init our default libraries
  L.runWith state L.openlibs

  pure ReplState
    { resolveScope = Bi.builtinResolve
    , inferScope   = Bi.builtinEnv
    , emitState    = B.defaultEmitState
    , lowerState   = L.defaultState
    , luaState     = state

    , lastName     = S.TgName (T.pack "a") 1
    , driver       = D.makeDriverWith (driverConfig config)
    , config       = config

    , currentFile  = Nothing
    , outputHandle = stdout
    }

resetState :: ReplState -> IO ReplState
resetState state = do
  lState <- L.newstate
  L.runWith lState L.openlibs
  pure state
    { resolveScope = Bi.builtinResolve
    , inferScope   = Bi.builtinEnv
    , emitState    = B.defaultEmitState
    , lowerState   = L.defaultState
    , luaState     = lState
    }
