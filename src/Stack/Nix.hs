{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Run commands in a nix-shell
module Stack.Nix
  (reexecWithOptionalShell
  ,nixCmdName
  ,nixHelpOptName
  ) where

import           Control.Arrow ((***))
import           Control.Exception (Exception,throw)
import           Control.Monad hiding (mapM)
import           Control.Monad.Catch (MonadMask)
import           Control.Monad.IO.Class (MonadIO,liftIO)
import           Control.Monad.Logger (MonadLogger,logDebug)
import           Control.Monad.Reader (MonadReader,asks)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.List (intercalate)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Data.Traversable
import           Data.Typeable (Typeable)
import           Data.Version (showVersion)
import           Network.HTTP.Client.Conduit (HasHttpManager)
import           Path
import           Path.IO
import qualified Paths_stack as Meta
import           Prelude hiding (mapM) -- Fix redundant import warnings
import           Stack.Config (makeConcreteResolver, getInNixShell, getInContainer)
import           Stack.Config.Nix (nixCompiler)
import           Stack.Constants (platformVariantEnvVar,inNixShellEnvVar)
import           Stack.Docker (reExecArgName)
import           Stack.Exec (exec)
import           Stack.Types
import           Stack.Types.Internal
import           System.Environment (getArgs,getExecutablePath)
import           System.Process.Read (getEnvOverride)

-- | If Nix is enabled, re-runs the currently running OS command in a Nix container.
-- Otherwise, runs the inner action.
reexecWithOptionalShell
    :: M env m
    => Maybe (Path Abs Dir)
    -> Maybe AbstractResolver
    -> Maybe CompilerVersion
    -> IO ()
    -> m ()
reexecWithOptionalShell mprojectRoot maresolver mcompiler inner =
  do config <- asks getConfig
     inShell <- getInNixShell
     inContainer <- getInContainer
     isReExec <- asks getReExec
     let getCmdArgs = do
           origArgs <- liftIO getArgs
           let args | inContainer = origArgs  -- internal-re-exec version already passed
                      -- first stack when restarting in the container
                    | otherwise =
                        ("--" ++ reExecArgName ++ "=" ++ showVersion Meta.version) : origArgs
           exePath <- liftIO getExecutablePath
           return (exePath, args)
     if nixEnable (configNix config) && not inShell && (not isReExec || inContainer)
        then runShellAndExit mprojectRoot maresolver mcompiler getCmdArgs
        else liftIO inner
    

runShellAndExit
    :: M env m
    => Maybe (Path Abs Dir)
    -> Maybe AbstractResolver
    -> Maybe CompilerVersion
    -> m (String, [String])
    -> m ()
runShellAndExit mprojectRoot maresolver mcompiler getCmdArgs = do
     config <- asks getConfig
     mresolver <- mapM makeConcreteResolver maresolver
     envOverride <- getEnvOverride (configPlatform config)
     (cmnd,args) <- fmap (escape *** map escape) getCmdArgs
     mshellFile <-
         traverse (resolveFile (fromMaybeProjectRoot mprojectRoot)) $
         nixInitFile (configNix config)
     let pkgsInConfig = nixPackages (configNix config)
         ghc = nixCompiler config mresolver mcompiler
         pkgs = pkgsInConfig ++ [ghc]
         pureShell = nixPureShell (configNix config)
         nixopts = case mshellFile of
           Just fp -> [toFilePath fp, "--arg", "ghc"
                      ,"with (import <nixpkgs> {}); " ++ T.unpack ghc]
           Nothing -> ["-E", T.unpack $ T.intercalate " " $ concat
                              [["with (import <nixpkgs> {});"
                               ,"runCommand \"myEnv\" {"
                               ,"buildInputs=lib.optional stdenv.isLinux glibcLocales ++ ["],pkgs,["];"
                               ,T.pack platformVariantEnvVar <> "=''nix'';"
                               ,T.pack inNixShellEnvVar <> "=1;"
                               ,"STACK_IN_NIX_EXTRA_ARGS=''"]
                               ,      (map (\p -> T.concat
                                                  ["--extra-lib-dirs=${",p,"}/lib"
                                                  ," --extra-include-dirs=${",p,"}/include "])
                                           pkgs), ["'' ;"
                               ,"} \"\""]]]
                    -- glibcLocales is necessary on Linux to avoid warnings about GHC being incapable to set the locale.
         fullArgs = concat [if pureShell then ["--pure"] else [],
                            map T.unpack (nixShellOptions (configNix config))
                           ,nixopts
                           ,["--run", intercalate " " (cmnd:"$STACK_IN_NIX_EXTRA_ARGS":args)]
                           ]
                           -- Using --run instead of --command so we cannot
                           -- end up in the nix-shell if stack build is Ctrl-C'd
     $logDebug $
         "Using a nix-shell environment " <> (case mshellFile of
            Just path -> "from file: " <> (T.pack (toFilePath path))
            Nothing -> "with nix packages: " <> (T.intercalate ", " pkgs))
     exec envOverride "nix-shell" fullArgs

-- | Shell-escape quotes inside the string and enclose it in quotes.
escape :: String -> String
escape str = "'" ++ foldr (\c -> if c == '\'' then
                                   ("'\"'\"'"++)
                                 else (c:)) "" str
                 ++ "'"

-- | Fail with friendly error if project root not set.
fromMaybeProjectRoot :: Maybe (Path Abs Dir) -> Path Abs Dir
fromMaybeProjectRoot = fromMaybe (throw CannotDetermineProjectRootException)

-- | Command-line argument for "nix"
nixCmdName :: String
nixCmdName = "nix"

nixHelpOptName :: String
nixHelpOptName = nixCmdName ++ "-help"

-- | Exceptions thrown by "Stack.Nix".
data StackNixException
  = CannotDetermineProjectRootException
    -- ^ Can't determine the project root (location of the shell file if any).
  deriving (Typeable)

instance Exception StackNixException

instance Show StackNixException where
  show CannotDetermineProjectRootException =
    "Cannot determine project root directory."

type M env m =
  (MonadIO m
  ,MonadReader env m
  ,MonadLogger m
  ,MonadBaseControl IO m
  ,MonadMask m
  ,HasConfig env
  ,HasTerminal env
  ,HasReExec env
  ,HasHttpManager env
  )
