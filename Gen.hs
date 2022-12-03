{-# LANGUAGE OverloadedStrings #-}

module Main where

import OGDFIDL
  ( cabal,
    classes,
    extraDep,
    extraLib,
    headers,
    templates,
    toplevelfunctions,
  )
import qualified Data.HashMap.Strict as HM
import FFICXX.Generate.Builder (simpleBuilder)
import FFICXX.Generate.Config
  ( FFICXXConfig (..),
    SimpleBuilderConfig (..),
  )
import FFICXX.Generate.Type.Config
  ( ModuleUnit (..),
    ModuleUnitImports (..),
    ModuleUnitMap (..),
    modImports,
  )
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))

main :: IO ()
main = do
  args <- getArgs
  let tmpldir =
        if length args == 1
          then args !! 0
          else "../template"

  cwd <- getCurrentDirectory
  let fficfg =
        FFICXXConfig
          { fficxxconfig_workingDir = cwd </> "tmp" </> "working",
            fficxxconfig_installBaseDir = cwd </> "OGDF",
            fficxxconfig_staticFileDir = tmpldir
          }
      sbcfg =
        SimpleBuilderConfig
          { sbcTopModule = "OGDF",
            sbcModUnitMap = ModuleUnitMap (HM.fromList headers),
            sbcCabal = cabal,
            sbcClasses = classes,
            sbcTopLevels = toplevelfunctions,
            sbcTemplates = templates,
            sbcExtraLibs = extraLib,
            sbcCxxOpts = ["-std=c++17"],
            sbcExtraDeps = extraDep,
            sbcStaticFiles = ["LICENSE"]
          }
  simpleBuilder fficfg sbcfg
