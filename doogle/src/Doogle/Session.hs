{-# LANGUAGE BlockArguments #-}

module Doogle.Session where

import Agda.Interaction.CommandLine (ReplM (ReplM), checkCurrentFile, loadFile, parseExpr)
import Agda.Interaction.Imports (crInterface)
import Agda.Interaction.Monad (readline)
import Agda.Syntax.Common.Pretty (Mode (OneLineMode), Style (Style, lineLength, mode, ribbonsPerLine), renderStyle)
import Agda.Syntax.Concrete.Name (NamePart (Id), QName (QName), nameNameParts)
import Agda.Syntax.Internal (Level' (Max), Sort' (Univ))
import Agda.Syntax.Internal.Univ (Univ (..))
import Agda.Syntax.Scope.Base (emptyScopeInfo)
import Agda.Syntax.Translation.ConcreteToAbstract (importPrimitives)
import Agda.TheTypeChecker (inferExpr)
import Agda.TypeChecking.Errors (renderError)
import Agda.TypeChecking.Monad (Closure (clValue), TCErr (tcErrClosErr), TCM, TypeError (NotInScope), iInsideScope, liftTCM, setScope, ExpandHidden (ExpandLast), Comparison (CmpLeq))
import Agda.TypeChecking.Monad.State (localScope)
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.TypeChecking.Rules.Term (isType, isType_, isType', inferExpr')
import Agda.Utils.CallStack
import Agda.Utils.FileName (AbsolutePath)
import Agda.Utils.List1 (NonEmpty ((:|)))
import Control.Monad (unless, void, when)
import Control.Monad.Error.Class (MonadError (catchError))
import Control.Monad.Trans (lift, liftIO)
import Data.Function ((&))
import Data.Maybe (isNothing)
import Doogle.Setup (setPreludeScope)
import Text.Printf (printf)

reload :: ReplM ()
reload = do
    checked <- checkCurrentFile
    liftTCM $ setScope $ maybe emptyScopeInfo (iInsideScope . crInterface) checked

    when (isNothing checked) $ do
        void (liftTCM importPrimitives)

    liftTCM $ setScope emptyScopeInfo

importProject :: String -> ReplM ()
importProject root = do
    -- the repl's `load` function doesn't actually load anything into scope, it sets the env's current file and evaluates the following
    -- exprs there.

    -- so either:
    -- 1. Define a synthetic file where I can just shove all the imports, such that everything is now in scope (or load a prelude file, if it exists).
    -- 2. Read up on how `TCM` brings things into scope (i.e, somehow, I'd need to load the interface files and/or figure out how to parse on my own).

    -- let's just go with 1 for now.

    loadFile reload [root]

-- pure ()

-- doogleInteractor :: AbsolutePath ->
doogleInteractor :: (HasCallStack) => AbsolutePath -> ReplM ()
doogleInteractor root = do
    -- importProject root
    liftTCM $ setPreludeScope root
    loop searchForTerm
  where
    loop :: (HasCallStack) => (String -> TCM ()) -> ReplM ()
    loop act = do
        line' <- ReplM $ lift $ lift $ readline "Doogle>"
        case line' of
            Nothing -> pure ()
            Just line -> do
                unless (null line) (liftTCM $ localScope $ attemptOrRepair line act)
                loop act

    attemptOrRepair line act = do
        catchError (liftTCM $ localScope $ act line) $ \err ->
            case err & tcErrClosErr & clValue of
                NotInScope (QName name') -> do
                    let Id name :| [] = nameNameParts name' -- TODO: Deal with other cases.
                    let augQuery = printf "(%s : _) -> %s" name line :: String
                    liftIO (putStrLn $ printf "Scope Error: %s is not in scope. Inserting it as a parameter." (show name))
                    liftIO (putStrLn $ printf "Repaired Query: %s" augQuery)
                    attemptOrRepair augQuery act
                _otherCause -> do
                    fmtErr <- renderError err
                    liftIO (putStrLn $ printf "Error: %s\n%s" (show err) fmtErr)

-- TODO: This should return a list of results.
searchForTerm :: (HasCallStack) => String -> TCM ()
searchForTerm line = do
    expr <- parseExpr line
    -- pprExpr <- prettyTCM expr
    -- liftIO (putStrLn $ renderStyle style pprExpr)
    
    (tpe, _ttpe) <- inferExpr' ExpandLast expr
    pprTpe <- prettyTCM tpe

    liftIO (print tpe)
    liftIO (putStrLn $ renderStyle style pprTpe)

    pure ()
  where
    style = Style{mode = OneLineMode, lineLength = 80, ribbonsPerLine = 1.0}