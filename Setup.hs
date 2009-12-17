import Distribution.Simple
import Distribution.Simple.InstallDirs
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription

import System

-- After Epic is built, we need a run time system.

-- FIXME: This is probably all done the wrong way, I don't really understand
-- Cabal properly...

buildLib args flags desc local 
    = do exit <- system "make -C evm"
         return ()

-- This is a hack. I don't know how to tell cabal that a data file needs
-- installing but shouldn't be in the distribution. And it won't make the
-- distribution if it's not there, so instead I just delete
-- the file after configure.

postConfLib args flags desc local
    = do exit <- system "make -C evm clean"
         return ()

addPrefix pfx var c = "export " ++ var ++ "=" ++ show pfx ++ "/" ++ c ++ ":$" ++ var

postInstLib args flags desc local
    = do let pfx = prefix (installDirTemplates local)
         exit <- system $ "make -C evm install PREFIX=" ++ show pfx
         return ()

main = defaultMainWithHooks (simpleUserHooks { postBuild = buildLib,
                                               postConf = postConfLib,
                                               postInst = postInstLib })

