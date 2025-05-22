module Main where

import Agda.TypeChecking.Monad.Base (runTCMTop)

-- https://hackage.haskell.org/package/Agda-2.7.0.1/docs/Agda-TypeChecking-Monad-Base.html#t:Interface
-- https://hackage.haskell.org/package/Agda-2.7.0.1/docs/Agda-TypeChecking-Monad-Base.html#t:Definition
-- https://hackage.haskell.org/package/Agda-2.7.0.1/docs/Agda-TypeChecking-Monad-Base.html#t:Defn

import Agda.Interaction.CommandLine (runReplM)
import Agda.Main (runTCMPrettyErrors)
import Agda.Utils.CallStack (HasCallStack)
import Agda.Utils.FileName (absolute)
import Control.Monad.IO.Class (liftIO)
import Doogle.Index (dumpDefs)
import Doogle.Options
import Doogle.Session (doogleInteractor)
import Doogle.Setup (loadPrelude, setupTCM)

buildIndex :: (HasCallStack) => String -> IO ()
buildIndex dir = do
    defs <- runTCMTop (dumpDefs dir)
    case defs of
        Left err -> print err
        Right defs' -> print defs'

runInteractive :: (HasCallStack) => String -> IO ()
runInteractive path = do
    runTCMPrettyErrors $ do
        _ <- setupTCM path
        abspath <- liftIO (absolute path)
        runReplM Nothing (pure ()) loadPrelude (doogleInteractor abspath)

main :: (HasCallStack) => IO ()
main = do
    opts <- parseCliOptions
    case opts of
        BuildIndex dir ->
            -- TODO: Check if the directory exists
            buildIndex dir
        Interactive prl -> runInteractive prl
        JsonInteractive -> error "todo: JSON interactive mode"

{-
There are two mechanisms we need to use to go about this

1. Interface Files
  - Go through an entire agda library (directory) (should we iterate over src or interface?)
  - Use `decodeFile` on each to get the interface for the file (if it doesnt exist, what should we do?)
  - extract the Signature of the model, it contains a field for all kinds of declarations.
  - Use Hoogle's way of matching signatures(?)

  Issue: Too large to fit all into memory to search through :/
         Search terms might need to be normalized/un-normalized

  Look at how Hoogle does it (binary database) for imprecise searches?

2. Local Searches
  - Agda already has an `interaction` that lets you search through the local scope of a module
  - Search terms mentioning names though, not types. _+_ "assoc"
  - Mechanism could be useful for precise searches

3. Loogle does something with the .olean files (essentially .agdai)
-}