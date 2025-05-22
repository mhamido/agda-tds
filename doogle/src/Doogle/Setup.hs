module Doogle.Setup where

import Agda.Compiler.Backend (AbsolutePath, SourceFile (SourceFile), TCM, setCommandLineOptions')
import Agda.Interaction.Imports (CheckResult, Mode (TypeCheck), crInterface, parseSource, typeCheckMain)
import Agda.Interaction.Options (CommandLineOptions (..), defaultOptions)
import Agda.TypeChecking.Monad (iInsideScope, setScope)
import Agda.Utils.FileId (idFromFile)
import Agda.Utils.FileName (absolute)
import Control.Monad.IO.Class (MonadIO (liftIO))
import System.FilePath (takeDirectory)

setupTCM :: FilePath -> TCM AbsolutePath
setupTCM path = do
    abspath <- liftIO $ absolute path
    dirname <- liftIO $ absolute $ takeDirectory path
    setCommandLineOptions' dirname opt
    pure abspath
  where
    opt :: CommandLineOptions
    opt = defaultOptions

loadPrelude :: AbsolutePath -> TCM CheckResult
loadPrelude abspath = do
    src <- idFromFile abspath >>= parseSource . SourceFile
    typeCheckMain TypeCheck src

setPreludeScope :: AbsolutePath -> TCM ()
setPreludeScope abspath = do
    prelude <- loadPrelude abspath
    let iface = crInterface prelude
    setScope (iInsideScope iface)