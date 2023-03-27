{-
  Setup.hs 

  Instead of using autoconf and configure, which assumes that dependencies
  will provide pkg-config (often they do not), the strategy here is
  to use R to configure. The user simply has to be sure that the
  desired version of R is in the search path. This permits use with
  HaskellR (and IHaskell for Jupyter) because 'stack --nix' places
  the Nix version of R (under /nix/) in the search path (Unix only).
  The 'HR' in module names like Foreign.HR.Types is used to prevent
  conflicts with HaskellR module names like Foreign.R.Types.

  To run the repl, use 'stack exec hr' (Windows) or
  'stack --nix exec hr' (Unix with Stack Nix integration). The script
  ~/.local/bin/runhr can be used to run Haskell scripts that use
  the library without having to specify the R shared library.

  By default the option -fno-ghci-sandbox is specified to
  prevent stack overflow, but note that this disables support
  for debugging with ghci. To run without this option (risking
  stack overflow), use 'runhr foo.hs d'. This enables debugging
  support.

  The configure file is a no-op (contains exit 0), and hooks defined
  here do what a configure file would normally do (configure based on
  architecture, OS, R version, etc.). Note that this requires a UNIX
  shell, normally part of MSYS (shipped with Stack), but also part of
  Rtools (shipped with R), and sometimes these shell versions are not
  substitutes for each other (may require some adjustments to PATH).

  The file hR.buildinfo is generated automatically by probing R
  for the necessary information, and this file is copied into
  the build tree under .stack-work (a subdirectory of the package
  root directory). I fresh build is done by
  deleting this build tree and using 'stack install' in the
  package root directory.
-}

import Distribution.Simple
import Distribution.Simple.Program
import Distribution.Simple.Program.Types
import qualified Distribution.Simple.Setup as S
import Distribution.Simple.Utils
import Distribution.PackageDescription
import Distribution.Verbosity
import Distribution.System
import Distribution.Types.LocalBuildInfo

import Control.Monad
import Data.Maybe
import System.IO
import System.Directory

-- | Find path to R binary.
-- Haskell library functions do not remove spaces in file paths
-- like R does (R returns short names that are shown under Windows
-- using DIR /X). Some older apps have problems with file names
-- containing spaces.
getConfiguredR :: IO ConfiguredProgram
getConfiguredR = do
  let rFind = findProgramOnSearchPath normal defaultProgramSearchPath "R"
  rFindResult <- rFind
  when (rFindResult == Nothing) $ rawSystemExit normal "R not found on PATH" []
  let rPath = fmap fst rFindResult
      rLoc = UserSpecified $ fromJust rPath
  return(simpleConfiguredProgram "R" rLoc)

-- | Find path to Unix shell (sh)
-- Typically finds the wrong version of sh.exe under Windows 
-- (Stack version instead of Rtools version), so we do
-- not use this function.
getConfiguredShell :: IO ConfiguredProgram
getConfiguredShell = do
  let rFind = findProgramOnSearchPath normal defaultProgramSearchPath "sh"
  rFindResult <- rFind
  when (rFindResult == Nothing) $ rawSystemExit normal "sh not found on PATH" []
  let rPath = fmap fst rFindResult
      rLoc = UserSpecified $ fromJust rPath
  return(simpleConfiguredProgram "sh" rLoc)

-- Get build directory used by Haskell Stack from ConfigFlags...
getBuildDir :: S.ConfigFlags -> FilePath
getBuildDir S.ConfigFlags { S.configDistPref=S.Flag path } = path++"/build"

-- One or more of these hooks can be overridden.
defhooks = autoconfUserHooks

-- main with a hook to call confhook below instead of the default confHook.
main = defaultMainWithHooks defhooks { confHook = confhook }

confhook :: (GenericPackageDescription, HookedBuildInfo) -> S.ConfigFlags -> IO LocalBuildInfo
confhook (g,b) conf = do
  putStrLn "Config Start"
  
  defaultReturn <-confHook defhooks (g,b) conf
  -- The first argument passed to confHook (defhooks) is present
  -- here because confHook is a field of UserHooks, so its first
  -- argument is the data structure that it is part of (not to be
  -- confused with the "this" pointer of C++ or Java!). 

  -- The imperative programmer might ask how can the original
  -- confHook be called when it was "replaced" by confhook above?
  -- The answer is that confHook has a different binding in the hooks
  -- data structure that is passed to defaultMainWithHooks above
  -- (the field assignment creates a new copy with a new value
  -- for confHooks), but this does not change its original 
  -- binding that is visible outside of that function (in this 
  -- function, in particular). Values are immutable in Haskell.
  
  -- Detect operating system and architecture...
  let platform = buildPlatform -- Platform Arch OS
      
  let os = case platform of
        Platform X86_64 Windows -> Windows
        Platform I386 Windows -> Windows
        Platform _ Linux -> Linux
        Platform _ OSX   -> OSX
        _                -> OtherOS "Unsupported"
  putStrLn $ "Detected OS: "++(show os)
      
  -- Get R command-line runnable interface...
  rConf <- getConfiguredR
  
  -- Get paths to be used for build/link/run...
  let rHomeInvoke = programInvocation rConf ["-e", "cat(R.home())", "--quiet","--slave"]
  rHomeDir <- getProgramInvocationOutput normal rHomeInvoke
  putStrLn $ "R home directory: " ++ rHomeDir
  
  let rCwdInvoke = programInvocation rConf ["-e", "cat(getwd())", "--quiet","--slave"]
  rCwdDir <- getProgramInvocationOutput normal rCwdInvoke
  
  let rUserHomeInvoke = programInvocation rConf ["-e", 
                                                 "cat(Sys.getenv('HOME'))", "--quiet","--slave"]
  rUserHomeDir <- getProgramInvocationOutput normal rUserHomeInvoke
  putStrLn $ "runhr.sh script written to: " ++ rUserHomeDir ++ "/.local/bin"
  
  let rCppFlagsInvoke = programInvocation rConf ["CMD", "config", 
                                                 "--cppflags", "--quiet", "--slave"]
  -- Under Windows the --cppflags option does not work because Stack
  -- changes PATH so a problematic sh.exe is used. For this reason
  -- we set include directory to $R_HOME/include under Windows.
  rCppUnix <- getProgramInvocationOutput normal rCppFlagsInvoke
  
  let rCppFlagsDir = if (os == Windows)
                     then rHomeDir ++ "/include" 
                     else drop 2 rCppUnix -- drop leading "-I"
      
  let binSuffix = case platform of
        Platform X86_64 Windows -> "/bin/x64"
        Platform I386 Windows   -> "/bin/i386"
        _                       -> "/lib"
        
  -- Cabal looks for hR.buildinfo (supplements build info in hR.cabal)
  -- in a build directory under .stack-work, not in the package
  -- source tree root directory. Get the build directory used from
  -- ConfigFlags...
  let buildDir = getBuildDir conf
      
  -- Write hR.buildinfo...
  handle <- openFile (buildDir ++ "/hR.buildinfo") WriteMode
  hPutStrLn handle $ "include-dirs: " ++ rCppFlagsDir
  hPutStrLn handle $ "extra-lib-dirs: " ++ rHomeDir ++ binSuffix
  hPutStrLn handle $ "extra-libraries: R"
  hClose handle
      
  -- Create ~/.local/bin dir if it doesn't exist...
  let dir1 = rUserHomeDir ++ "/.local"
  exists <- doesPathExist dir1
  if(not exists) then createDirectory dir1 else return ()
  let dir2 = dir1 ++ "/bin"
  exists <- doesPathExist dir2
  if(not exists) then createDirectory dir2 else return()
         
  let scriptSuffix = if(os == Windows) then ".sh" else ""

  -- Write ~/.local/bin/runhr (runhr.sh under Windows)...
  handle <- openFile (rUserHomeDir ++ "/.local/bin/runhr"++scriptSuffix) WriteMode
  hPutStrLn handle "if [ \"$1\" = \"\" ]; then"
  hPutStrLn handle "  echo \"Usage: $0 <ghci-script> [d]\""
  hPutStrLn handle "  exit 2"
  hPutStrLn handle "fi"
  hPutStrLn handle "if [ \"$2\" = \"d\" ]; then"
  hPutStrLn handle "  echo \"Debugging enabled (no -fno-ghci-sandbox)\""
  hPutStrLn handle "  stack exec ghci -- $1 \\"
  hPutStrLn handle $ ("    -L\""++rHomeDir++binSuffix++"\"")++" -lR"
  hPutStrLn handle "else"
  hPutStrLn handle "  echo \"Debugging disabled (-fno-ghci-sandbox)\""
  hPutStrLn handle "  stack exec ghci -- -fno-ghci-sandbox $1 \\"
  hPutStrLn handle $ ("    -L\""++rHomeDir++binSuffix++"\"")++" -lR"
  hPutStrLn handle "fi"
  hClose handle
  
  -- Under Windows write batch file wrapper to ~/.local/bin/runhr.bat.
  -- Assumes ~/.local/bin is on PATH, and env variables HOME and RTOOLS_HOME 
  -- are defined, with the latter pointing to the root of 
  -- Rtools, e.g., C:\Rtools.
  if(os == Windows)
    then do
      handle <- openFile (rUserHomeDir ++ "/.local/bin/runhr.bat") WriteMode
      hPutStrLn handle "%RTOOLS_HOME%/usr/bin/sh %HOME%/.local/bin/runhr.sh %*%"
      hClose handle
    else return ()
         
  putStrLn "Config Done"
  return defaultReturn
  
  
