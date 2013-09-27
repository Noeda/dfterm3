{-# LANGUAGE CPP #-}

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Program.Types
import Distribution.Verbosity
import Distribution.Simple.Utils

main = defaultMainWithHooks myHooks

myHooks :: UserHooks
myHooks = simpleUserHooks
    { preBuild = myPreBuild }

myPreBuild :: Args -> BuildFlags -> IO HookedBuildInfo
myPreBuild args bf = do
#ifdef mingw32_HOST_OS
    _ <- rawSystemExitCode normal "windres"
                  ["dfterm3.rc", "-O", "coff", "icon/icon.o"]
#endif
    (preBuild simpleUserHooks) args bf

