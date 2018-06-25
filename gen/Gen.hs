module Main where

import Data.Monoid (mempty)
--
import FFICXX.Generate.Builder
import FFICXX.Generate.Type.Class
import FFICXX.Generate.Type.Module
import FFICXX.Generate.Type.PackageInterface

-- TODO: Add this function to fficxx.
ref_ :: CTypes -> Types
ref_ t = CT (CRef t) NoConst


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


deletable :: Class
deletable =
  AbstractClass cabal "Deletable" [] mempty Nothing
  [ Destructor Nothing
  ]

string :: Class
string =
  Class cabal "string" [ deletable ] mempty  (Just "CppString")
  [ Constructor [ cstring "p" ] Nothing
  , NonVirtual cstring_ "c_str" [] Nothing
  ]


graph :: Class
graph =
  Class cabal "Graph" [ deletable ] mempty Nothing
  [ Constructor [] Nothing
  , NonVirtual (cppclass_ nodeElement) "newNode" [] Nothing
  , NonVirtual (cppclass_ nodeElement) "newNode" [int "index"] (Just "newNode1")
  ]



graphAttributes :: Class
graphAttributes =
  Class cabal "GraphAttributes" [ deletable ] mempty Nothing
  [ Constructor [ cppclassref graph "g", long "initAttributes" ] Nothing
  , NonVirtual (ref_ CTDouble) "x" [ cppclass nodeElement "v" ] Nothing
  , NonVirtual (ref_ CTDouble) "y" [ cppclass nodeElement "v" ] Nothing
  , NonVirtual (ref_ CTDouble) "width" [ cppclass nodeElement "v" ] Nothing
  , NonVirtual (ref_ CTDouble) "height" [ cppclass nodeElement "v" ] Nothing
  -- , Virtual (ref_ CTDouble) "x" [ cppclass nodeElement "v" ] (Just "gAX")
  ]

graphIO :: Class
graphIO =
  Class cabal "GraphIO" [ deletable ] mempty Nothing
  [ -- Static bool_ "readGML" [ cppclassref graph "g", cppclassref string "filename" ] Nothing
    Static bool_ "readGML" [ cppclassref graphAttributes "ga", cppclassref graph "g", cppclassref string "filename" ] Nothing
  -- , Static bool_ "writeGML" [ cppclassref graph "g", cppclassref string "filename" ] Nothing
  , Static bool_ "writeGML" [ cppclassref graphAttributes "ga", cppclassref string "filename" ] Nothing -- (Just "graphIOwriteGMLGA")
  ]

hierarchyLayoutModule :: Class
hierarchyLayoutModule =
  Class cabal "HierarchyLayoutModule" [ deletable ] mempty Nothing
  [
  ]


layerByLayerSweep :: Class
layerByLayerSweep =
  Class cabal "LayerByLayerSweep" [ deletable, layeredCrossMinModule ] mempty Nothing
  [
  ]


layeredCrossMinModule :: Class
layeredCrossMinModule =
  Class cabal "LayeredCrossMinModule" [ deletable ] mempty Nothing
  [
  ]

layoutModule :: Class
layoutModule =
  AbstractClass cabal "LayoutModule" [ deletable ] mempty Nothing
  [ Virtual void_ "call" [ cppclassref graphAttributes "ga" ] Nothing
  ]

medianHeuristic :: Class
medianHeuristic =
  Class cabal "MedianHeuristic" [ deletable, layerByLayerSweep ] mempty Nothing
  [ Constructor [] Nothing
  ]


nodeElement :: Class
nodeElement =
  Class cabal "NodeElement" [ deletable ] mempty Nothing
  [
  ]

optimalHierarchyLayout :: Class
optimalHierarchyLayout =
  Class cabal "OptimalHierarchyLayout" [ deletable, hierarchyLayoutModule ] mempty Nothing
  [ Constructor [] Nothing
  , NonVirtual void_ "layerDistance" [ double "x" ] Nothing
  , NonVirtual void_ "nodeDistance" [ double "x" ] Nothing
  , NonVirtual void_ "weightBalancing" [ double "w" ] Nothing
  ]

optimalRanking :: Class
optimalRanking =
  Class cabal "OptimalRanking" [ deletable, rankingModule ] mempty Nothing
  [ Constructor [] Nothing
  ]

rankingModule :: Class
rankingModule =
  Class cabal "RankingModule" [ deletable ] mempty Nothing
  [
  ]

sugiyamaLayout :: Class
sugiyamaLayout =
  Class cabal "SugiyamaLayout" [ deletable, layoutModule ] mempty Nothing
  [ Constructor [] Nothing
  , NonVirtual void_ "setCrossMin" [cppclass layeredCrossMinModule "pCrossMin"] Nothing
  , NonVirtual void_ "setLayout" [cppclass hierarchyLayoutModule "pLayout"] Nothing
  , NonVirtual void_ "setRanking" [cppclass rankingModule "pRanking"] Nothing
  ]

classes = [ deletable
          --
          , string
          --
          , graph, graphAttributes, graphIO
          , hierarchyLayoutModule
          , layerByLayerSweep, layeredCrossMinModule, layoutModule
          , medianHeuristic
          , nodeElement
          , optimalHierarchyLayout, optimalRanking
          , rankingModule
          , sugiyamaLayout
          ]

toplevelfunctions = [ ]

templates = [  ]

headerMap = [ ("string"       , ([NS "std"          ], [HdrName "string"   ]))
            , ("Graph"          , ([NS "ogdf"], [HdrName "ogdf/basic/Graph_d.h"]))
            , ("GraphAttributes", ([NS "ogdf"], [HdrName "ogdf/basic/GraphAttributes.h"]))
            , ("GraphIO"        , ([NS "ogdf"], [HdrName "ogdf/fileformats/GraphIO.h"]))
            , ("HierarchyLayoutModule", ([NS "ogdf"], [HdrName "ogdf/module/HierarchyLayoutModule.h"]))
            , ("LayerByLayerSweep" , ([NS "ogdf"], [HdrName "ogdf/module/LayerByLayerSweep.h"]))
            , ("LayeredCrossMinModule" , ([NS "ogdf"], [HdrName "ogdf/module/LayeredCrossMinModule.h"]))
            , ("LayoutModule"   , ([NS "ogdf"], [HdrName "ogdf/module/LayoutModule.h"]))

            , ("MedianHeuristic" , ([NS "ogdf"], [HdrName "ogdf/layered/MedianHeuristic.h"]))
            , ("NodeElement"     , ([NS "ogdf"], [HdrName "ogdf/basic/Graph_d.h"]))
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
