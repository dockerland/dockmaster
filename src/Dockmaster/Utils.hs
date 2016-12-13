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
    eitherWrap
  , testM
  -- * Sh and FilePath utils
  , getHomeDirectory
  , parsePath
  , toText
  ) where

import Shelly
import Prelude hiding (FilePath)
import qualified Filesystem.Path.CurrentOS as FP
import qualified Filesystem as F
import qualified Data.Text as T
default (T.Text)

-- | Basically fmap over Either, but allow two functions for each L/R side
--
-- Used for text packing on the error
eitherWrap :: (a -> b) -> (c -> d) -> Either a c -> Either b d
eitherWrap f _ (Left a)  = Left $ f a
eitherWrap _ g (Right c) = Right $ g c

-- | Just a contrived predicate for returning a maybe value within monad context
--
-- Used during the fallback directory structure
testM :: (Monad m) => (a -> m Bool) -> m a -> m (Maybe a)
testM predM mx = do
  test <- mx >>= predM
  x <- mx
  return $ if test then Just x else Nothing

-- | Accepts a path as 'Text' and returns a 'FilePath' path but with
-- @~@ and @$HOME@ replaced with user home directory
-- @$DOCKMASTER_HOME@ replaced with either env variable value
-- or defaults to @~/.dockmaster@
parsePath :: T.Text -> Sh FilePath
parsePath path = do
  home   <- getHomeDirectory >>= toText
  dmHome <- getDmHomeDirectory >>= toText
  let replace         = foldr (.) id $ map (\(old,new) -> T.replace old new) dirReplacements
      dirReplacements = [ ("~", home)
                        , ("$HOME", home)
                        , ("$DOCKMASTER_HOME", dmHome) ]
   in (return . fromText . replace) path

-- | Convert 'FilePath' to 'Text' within 'Sh', exits on failure to convert
toText :: FilePath -> Sh T.Text
toText fp = case FP.toText fp of
    Left err   -> errorExit err
    Right path -> return path

-- | Get home directory (in Sh)
getHomeDirectory :: Sh FilePath
getHomeDirectory = liftIO F.getHomeDirectory

-- | Get DM home directory (in Sh)
-- TODO !!!
getDmHomeDirectory :: Sh FilePath
getDmHomeDirectory = do
  home <- getHomeDirectory
  return $ FP.concat [home, fromText ".dockmaster"]
