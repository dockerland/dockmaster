{-|
Module      : Dockmaster.Utils
Description : Helper utilities
License     : ASL-2
Maintainer  : sam.chong.tay@gmail.com
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Dockmaster.Utils
  (
  -- * Utility methods for common types
    testM
  -- * Sh and FilePath utils
  , getHomeDirectory
  , getDmHomeDirectory
  , parsePath
  , parsePath'
  , toText
  , log
  , errorExit'
  , run'
  , run_'
  , bash_'
  , (</>>=)
  , (<</>>)
  ) where

import Shelly
import Control.Monad (liftM, liftM2, (>=>))
import Prelude hiding (FilePath, log)
import qualified Filesystem.Path.CurrentOS as FP
import qualified Filesystem as F
import qualified Data.Text as T
default (T.Text)

-- | Just a contrived predicate for returning a maybe value within monad context
--
-- Used during the fallback directory structure
testM :: (Monad m) => (a -> m Bool) -> m a -> m (Maybe a)
testM predM mx = do
  test <- mx >>= predM
  x <- mx
  return $ if test then Just x else Nothing

-- | Log messages to stdout (only logs when verbose is set)
--
-- Unfortunately 'Shelly.trace' will only output text on failure,
-- and 'Shelly.echo' prints stdout regardless of verbosity
log :: T.Text -> Sh ()
log msg = print_commands False $ run_ "echo" [msg]

-- | A version of Shelly's 'errorExit' without debug information
errorExit' :: T.Text -> Sh a
errorExit' err = echo_err err >> quietExit 1

-- | A version of Shelly's 'run' that avoids printing out a stack trace on error
--
-- Warning ! Without access 'sErrExit', this will always assume @'errExit' True@.
-- Issue open at https://github.com/yesodweb/Shelly.hs/issues/139
run' :: FilePath -> [T.Text] -> Sh T.Text
run' = mkRunner run

-- | A version of Shelly's 'run_' that avoids printing out a stack trace on error
run_' :: FilePath -> [T.Text] -> Sh ()
run_' = mkRunner run_

-- | A version of Shelly's 'bash_' that avoids printing out a stack trace on error
bash_' :: FilePath -> [T.Text] -> Sh ()
bash_' = mkRunner bash_

mkRunner :: (FilePath -> [T.Text] -> Sh a) -> FilePath -> [T.Text] -> Sh a
mkRunner run fp args = do
  -- e <- sErrExit <$> get
  let e = True
  stdo <- errExit False $ run fp args
  ex <- lastExitCode
  if e && ex > 0
     then let err = T.concat ["Error running: \"", show_command fp args, "\""]
           in echo_err err >> lastStderr >>= errorExit'
     else return stdo

-- | Accepts a path as 'Text' and returns a 'FilePath' path but with
-- @~@ and @$HOME@ replaced with user home directory, and
-- @$DOCKMASTER_HOME@ replaced with either env variable value
-- or defaults to @~/.dockmaster@
parsePath :: T.Text -> Sh FilePath
parsePath path = do
  home   <- getHomeDirectory >>= toText
  dmHome <- getDmHomeDirectory >>= toText
  let replace         = foldr (.) id $ map (uncurry T.replace) dirReplacements
      dirReplacements = [ ("~", home)
                        , ("$HOME", home)
                        , ("$DOCKMASTER_HOME", dmHome) ]
   in (return . fromText . replace) path

-- | Just like 'parsePath' but goes from 'FilePath' to 'Sh Text'
parsePath' :: FilePath -> Sh T.Text
parsePath' = toText >=> parsePath >=> toText

-- | Convert 'FilePath' to 'Text' within 'Sh', exits on failure to convert
toText :: FilePath -> Sh T.Text
toText fp = case FP.toText fp of
    Left err   -> errorExit err
    Right path -> return path

-- | Get home directory
getHomeDirectory :: Sh FilePath
getHomeDirectory = liftIO F.getHomeDirectory

-- | Get dockmaster home directory
--
-- Prefers @DOCKMASTER_HOME@ environment variable if set, otherwise defaults
-- to @~/.dockmaster@.
getDmHomeDirectory :: Sh FilePath
getDmHomeDirectory = do
  envDir <- get_env "DOCKMASTER_HOME"
  maybe
    (getHomeDirectory </>>= ".dockmaster")
    (return . fromText)
    envDir

-- | Convenience method to append filepaths when one is wrapped in a monad
infixr 4 </>>=
(</>>=) :: (Monad m) => m FilePath -> FilePath -> m FilePath
mFp </>>= fp = mFp <</>> return fp

-- | Convenience method to append filepaths when both are wrapped in a monad
infixr 5 <</>>
(<</>>) :: (Monad m) => m FilePath -> m FilePath -> m FilePath
(<</>>) = liftM2 (</>)
