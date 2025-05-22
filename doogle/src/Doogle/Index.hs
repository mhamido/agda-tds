{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Doogle.Index where

import Agda.Interaction.FindFile (mkInterfaceFile)
import Agda.Interaction.Imports (readInterface)
import Agda.Syntax.Abstract (ModuleName)
import Agda.Syntax.Abstract.Name (QName)
import Agda.Syntax.Internal (Type)
import Agda.TypeChecking.Monad (Interface, Signature (_sigDefinitions), TCM)
import Agda.TypeChecking.Monad qualified
import Agda.TypeChecking.Monad.Base (Definition)
import Agda.Utils.FileName (absolute)
import Control.Monad.Trans (lift)
import System.Directory.Recursive as Dir

import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap

import Agda.Syntax.Common.Pretty (prettyShow)
import Doogle.Utils (catMaybes', hasExtension)
import Text.Printf (printf)

dumpDefs :: FilePath -> TCM [(ModuleName, HashMap QName (Definition, Type))]
dumpDefs root = do
    -- TODO: This is a hack to grab interface files assuming they already exist.
    -- It would be better to get the interface from source files and create them if they don't.
    -- But, issues arise, and I can't get `findIinterfaceFile'` to locate them.

    {- srcpaths <- lift (Dir.getFilesRecursive root >>= traverse (fmap SourceFile . absolute) . filter (hasExtension "agdai")) -}

    abspaths <- lift (Dir.getFilesRecursive root >>= traverse absolute . filter (hasExtension "agdai"))
    ifcpaths <- lift $ catMaybes' mkInterfaceFile (putStrLn . printf "Could not `mkInterfaceFile` for %s" . show) abspaths
    interfaces <- catMaybes' readInterface (lift . putStrLn . printf "Could not `readinterface` for %s" . prettyShow) ifcpaths

    lift (putStrLn $ printf "There are %d interface files loaded, and %d located." (length interfaces) (length abspaths))

    -- ipaths <- catMaybes' findInterfaceFile' (lift . putStrLn . printf "Could not find an interface file for \"%s\"." . show) srcpaths
    -- _ <- trace (show $ length ifaces) $ pure ()
    -- ipaths <- catMaybes <$> lift (traverse mkInterfaceFile abspaths)
    -- https://hackage.haskell.org/package/Agda-2.7.0.1/docs/src/Agda.Interaction.FindFile.html#findInterfaceFile%27

    -- TODO: It's probably not a good idea to keep these in memory, especially when doing this on a large package.
    traverse fetchDefs interfaces

fetchDefs :: Interface -> TCM (ModuleName, HashMap QName (Definition, Type))
fetchDefs iface = pure (name, defsTypes)
  where
    name = Agda.TypeChecking.Monad.iModuleName iface
    sig = Agda.TypeChecking.Monad.iSignature iface
    defs = _sigDefinitions sig
    defsTypes = HashMap.map (\def -> (def, Agda.TypeChecking.Monad.defType def)) defs

-- moduleContents AsIs NoRange moduleName :: TCM ([Name], Telescope, [(Name, Type)])
-- https://hackage.haskell.org/package/Agda-2.7.0.1/docs/src/Agda.Interaction.BasicOps.html#moduleContents