{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Array (listArray)
import Data.Function (on)
import qualified Data.Graph as G
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.Tree (drawForest)
import FFICXX.Generate.Builder (simpleBuilder)
import FFICXX.Generate.Config
  ( FFICXXConfig (..),
    SimpleBuilderConfig (..),
  )
import FFICXX.Generate.Dependency.Graph (constructDepGraph)
import FFICXX.Generate.Type.Config
  ( ModuleUnit (..),
    ModuleUnitImports (..),
    ModuleUnitMap (..),
    modImports,
  )
import FFICXX.Generate.Type.Module
  ( TemplateClassImportHeader (..),
  )
import FFICXX.Generate.Util.DepGraph (drawDepGraph)
import OGDFIDL
  ( cabal,
    classes,
    extraDep,
    extraLib,
    headers,
    templates,
    toplevelfunctions,
  )
import qualified Options.Applicative as OA
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), hPutStrLn, stdout, withFile)

data CLIMode
  = Gen (Maybe FilePath)
  | DepGraph (Maybe FilePath)

genMode :: OA.Mod OA.CommandFields CLIMode
genMode =
  OA.command "gen" $
    OA.info
      ( Gen
          <$> OA.optional
            (OA.strOption (OA.long "template" <> OA.short 't' <> OA.help "template directory"))
      )
      (OA.progDesc "Generate source code")

depGraphMode :: OA.Mod OA.CommandFields CLIMode
depGraphMode =
  OA.command "depgraph" $
    OA.info
      ( DepGraph
          <$> OA.optional
            (OA.strOption (OA.long "dotfile" <> OA.short 'f' <> OA.help "output dot file"))
      )
      (OA.progDesc "Generate dependency graph")

optsParser :: OA.ParserInfo CLIMode
optsParser =
  OA.info
    (OA.subparser (genMode <> depGraphMode) OA.<**> OA.helper)
    OA.fullDesc

main :: IO ()
main = do
  mode <- OA.execParser optsParser
  case mode of
    Gen mtmplDir -> do
      let tmplDir = fromMaybe "../template" mtmplDir
      cwd <- getCurrentDirectory
      let fficfg =
            FFICXXConfig
              { fficxxconfig_workingDir = cwd </> "tmp" </> "working",
                fficxxconfig_installBaseDir = cwd </> "OGDF",
                fficxxconfig_staticFileDir = tmplDir
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
    DepGraph mdotFile -> do
      let allclasses = fmap Right classes <> fmap (Left . tcihTClass) templates
          drawAction h = do
            hPutStrLn h $
              drawDepGraph allclasses toplevelfunctions
      {-
        let (syms, m) = constructDepGraph allclasses toplevelfunctions
            n = length syms
            bounds = (0, n - 1)
            gr = listArray bounds $ fmap (\i -> fromMaybe [] (L.lookup i m)) [0..n-1]
        putStrLn $ drawForest (fmap (fmap show) (G.scc gr))
      -}
      case mdotFile of
        Nothing -> drawAction stdout
        Just dotFile -> withFile dotFile WriteMode drawAction
