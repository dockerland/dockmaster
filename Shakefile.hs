#!/usr/bin/env stack
{- stack --resolver lts-7 --install-ghc
     runghc
     --package shake
     --
     -rtsopts -with-rtsopts=-I0
-}
import Development.Shake
import Development.Shake.FilePath

shakeOpts :: ShakeOptions
shakeOpts = shakeOptions { shakeFiles = ".shake/" }

stackBuildOpts :: [String]
stackBuildOpts = ["--copy-bins", "--local-bin-path", "dist"]

srcFiles :: Action [FilePath]
srcFiles = getDirectoryFiles "" ["src//*.hs"]

main :: IO ()
main = shakeArgs shakeOpts $ do
  want ["dist/dm" <.> exe]

  "dist/dm" <.> exe %> \f -> do
    need <$> ("app/Dm.hs":) <$> srcFiles
    cmd "stack build dockmaster:exe:dm" stackBuildOpts

  "dist/dmc" <.> exe %> \f -> do
    need <$> ("app/Dmc.hs":) <$> srcFiles
    cmd "stack build dockmaster:exe:dmc" stackBuildOpts

  "clean" ~> do
    putNormal "Cleaning files in .shake/ and dist/"
    removeFilesAfter ".shake" ["//*"]
    removeFilesAfter "dist" ["//*"]

  "tests" ~> do
    putNormal "Running app-level tests via stack"
    cmd "stack test"
