module Revna.Typer.Env
  ( TcM (..),
    Typer,
    runLocalCtx,
    extendPos,
    getContext,
    getCurrPos,
    emitDiagnostic,
    runTc,
  )
where

import Control.Monad.Except
import Control.Monad.Reader
import Debug.Trace (traceM)
import Revna.Context (Context, emptyContext)
import Revna.Diagnostic
import Revna.Location

data Env = Env
  { ctx :: Context,
    currPos :: Span
  }
  deriving (Show)

type Typer m = (MonadReader Env m, MonadError Diagnostic m)

newtype TcM a = TcM
  { unTC :: ReaderT Env (Except Diagnostic) a
  }
  deriving (Functor, Applicative, Monad, MonadError Diagnostic)

runTc :: TcM a -> Either Diagnostic a
runTc action =
  let env = Env emptyContext (Span (startPos "x") (startPos "x"))
      x = runReaderT (unTC action) env
   in case runExcept x of
        Left err -> Left err
        Right a -> Right a

runLocalCtx :: Typer m => (Context -> Context) -> m a -> m a
runLocalCtx f = local (\env -> env {ctx = f (ctx env)})

extendPos :: Typer m => Span -> m a -> m a
extendPos sp = local (\env -> env {currPos = sp})

getContext :: Typer m => m Context
getContext = asks ctx

getCurrPos :: Typer m => m Span
getCurrPos = asks currPos

emitDiagnostic :: Typer m => Severity -> [String] -> m a
emitDiagnostic sv sn = do
  sp <- getCurrPos
  ctx' <- getContext
  traceM ("[Context]: \n" <> show ctx')
  throwError $
    Diagnostic
      { severity = sv,
        snippets = map (Snippet sp) sn,
        phase = Typing
      }
