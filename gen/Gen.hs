module Main where

import Data.Monoid (mempty)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface

cabal = Cabal { cabal_pkgname = "OGDF"
              , cabal_cheaderprefix = "OGDF"
              , cabal_moduleprefix = "OGDF"
              , cabal_additional_c_incs = []
              , cabal_additional_c_srcs = []
              }

extraDep = []

cabalattr =
    CabalAttr
    { cabalattr_license = Just "BSD3"
    , cabalattr_licensefile = Just "LICENSE"
    , cabalattr_extraincludedirs = [ ]
    , cabalattr_extralibdirs = []
    , cabalattr_extrafiles = []
    }

graph :: Class
graph =
  Class cabal "Graph" [] mempty Nothing
  [ Constructor [] Nothing
  ]

graphAttributes :: Class
graphAttributes =
  Class cabal "GraphAttributes" [] mempty Nothing
  [ Constructor [ cppclassref graph "g", long "initAttributes" ] Nothing
  ]

graphIO :: Class
graphIO =
  Class cabal "GraphIO" [] mempty Nothing
  [ Static bool_ "readGML" [ cppclassref graph "g", cstring "filename" ] Nothing
  ]

optimalRanking :: Class
optimalRanking =
  Class cabal "OptimalRanking" [rankingModule] mempty Nothing
  [ Constructor [] Nothing
  ]

rankingModule :: Class
rankingModule =
  Class cabal "RankingModule" [] mempty Nothing
  [
  ]

sugiyamaLayout :: Class
sugiyamaLayout =
  Class cabal "SugiyamaLayout" [] mempty Nothing
  [ Constructor [] Nothing
  , NonVirtual void_ "setRanking" [cppclass rankingModule "pRanking"] Nothing
  ]

classes = [ graph, graphAttributes, graphIO
          , optimalRanking
          , rankingModule
          , sugiyamaLayout
          ]

toplevelfunctions = [ ]

templates = [  ]

headerMap = [ ("Graph"          , ([NS "ogdf"], [HdrName "ogdf/basic/Graph_d.h"]))
            , ("GraphAttributes", ([NS "ogdf"], [HdrName "ogdf/basic/GraphAttributes.h"]))
            , ("GraphIO"        , ([NS "ogdf"], [HdrName "ogdf/fileformats/GraphIO.h"]))
            , ("OptimalRanking" , ([NS "ogdf"], [HdrName "ogdf/layered/OptimalRanking.h"]))
            , ("RankingModule"  , ([NS "ogdf"], [HdrName "ogdf/module/RankingModule.h"]))
            , ("SugiyamaLayout" , ([NS "ogdf"], [HdrName "ogdf/layered/SugiyamaLayout.h"]))
            ]

main :: IO ()
main = do
  simpleBuilder
    "OGDF"
    headerMap
    (cabal,cabalattr,classes,toplevelfunctions,templates)
    [ "OGDF", "COIN" ]
    extraDep
