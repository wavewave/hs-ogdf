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


string :: Class
string =
  Class cabal "string" [] mempty  (Just "CppString")
  [ Constructor [ cstring "p" ] Nothing
  , NonVirtual cstring_ "c_str" [] Nothing
  , Destructor Nothing
  ]


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
  [ Static bool_ "readGML" [ cppclassref graph "g", cppclassref string "filename" ] Nothing
  , Static bool_ "writeGML" [ cppclassref graph "g", cppclassref string "filename" ] Nothing
  ]

hierarchyLayoutModule :: Class
hierarchyLayoutModule =
  Class cabal "HierarchyLayoutModule" [] mempty Nothing
  [
  ]


layerByLayerSweep :: Class
layerByLayerSweep =
  Class cabal "LayerByLayerSweep" [ layeredCrossMinModule ] mempty Nothing
  [
  ]


layeredCrossMinModule :: Class
layeredCrossMinModule =
  Class cabal "LayeredCrossMinModule" [] mempty Nothing
  [
  ]

layoutModule :: Class
layoutModule =
  AbstractClass cabal "LayoutModule" [] mempty Nothing
  [ Virtual void_ "call" [ cppclassref graphAttributes "ga" ] Nothing
  ]

medianHeuristic :: Class
medianHeuristic =
  Class cabal "MedianHeuristic" [ layerByLayerSweep ] mempty Nothing
  [ Constructor [] Nothing
  ]

optimalHierarchyLayout :: Class
optimalHierarchyLayout =
  Class cabal "OptimalHierarchyLayout" [ hierarchyLayoutModule ] mempty Nothing
  [ Constructor [] Nothing
  , NonVirtual void_ "layerDistance" [ double "x" ] Nothing
  , NonVirtual void_ "nodeDistance" [ double "x" ] Nothing
  , NonVirtual void_ "weightBalancing" [ double "w" ] Nothing
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
  Class cabal "SugiyamaLayout" [ layoutModule ] mempty Nothing
  [ Constructor [] Nothing
  , NonVirtual void_ "setCrossMin" [cppclass layeredCrossMinModule "pCrossMin"] Nothing
  , NonVirtual void_ "setLayout" [cppclass hierarchyLayoutModule "pLayout"] Nothing
  , NonVirtual void_ "setRanking" [cppclass rankingModule "pRanking"] Nothing
  ]

classes = [ string
          , graph, graphAttributes, graphIO
          , hierarchyLayoutModule
          , layerByLayerSweep, layeredCrossMinModule, layoutModule
          , medianHeuristic
          , optimalHierarchyLayout, optimalRanking
          , rankingModule
          , sugiyamaLayout
          ]

toplevelfunctions = [ ]

templates = [  ]

headerMap = [ ("Graph"          , ([NS "ogdf"], [HdrName "ogdf/basic/Graph_d.h"]))
            , ("GraphAttributes", ([NS "ogdf"], [HdrName "ogdf/basic/GraphAttributes.h"]))
            , ("GraphIO"        , ([NS "ogdf"], [HdrName "ogdf/fileformats/GraphIO.h"]))
            , ("HierarchyLayoutModule", ([NS "ogdf"], [HdrName "ogdf/module/HierarchyLayoutModule.h"]))
            , ("LayerByLayerSweep" , ([NS "ogdf"], [HdrName "ogdf/module/LayerByLayerSweep.h"]))
            , ("LayeredCrossMinModule" , ([NS "ogdf"], [HdrName "ogdf/module/LayeredCrossMinModule.h"]))
            , ("LayoutModule"   , ([NS "ogdf"], [HdrName "ogdf/module/LayoutModule.h"]))

            , ("MedianHeuristic" , ([NS "ogdf"], [HdrName "ogdf/layered/MedianHeuristic.h"]))
            , ("OptimalHierarchyLayout" , ([NS "ogdf"], [HdrName "ogdf/layered/OptimalHierarchyLayout.h"]))
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
