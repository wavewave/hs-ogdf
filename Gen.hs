{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashMap.Strict as HM
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
--
import FFICXX.Generate.Builder        ( simpleBuilder )
import FFICXX.Generate.Code.Primitive ( bool_
                                      , charpp
                                      , cppclass, cppclass_
                                      , cppclassref, cppclassref_
                                      , cstring, cstring_
                                      , double, double_
                                      , int, int_
                                      , long
                                      , ref_
                                      , self_
                                      , uint, uint_
                                      , void_, voidp
                                      )
import FFICXX.Generate.Config         ( FFICXXConfig(..)
                                      , SimpleBuilderConfig(..)
                                      )
import FFICXX.Generate.Type.Cabal     ( BuildType(..), Cabal(..), CabalName(..) )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..), ModuleUnitMap(..), ModuleUnitImports(..)
                                      , modImports
                                      )
import FFICXX.Generate.Type.Class     ( Class(..)
                                      , ClassAlias(..)
                                      , CTypes(CTDouble)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TopLevel(..)
                                      , Variable(..)
                                      )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..)
                                      , ModuleUnitImports(..)
                                      )


-- -------------------------------------------------------------------
-- import from stdcxx
-- -------------------------------------------------------------------

-- import from stdcxx
stdcxx_cabal = Cabal {
    cabal_pkgname            = CabalName "stdcxx"
  , cabal_version            = "0.5"
  , cabal_cheaderprefix      = "STD"
  , cabal_moduleprefix       = "STD"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = []
  , cabal_license            = Nothing
  , cabal_licensefile        = Nothing
  , cabal_extraincludedirs   = []
  , cabal_extralibdirs       = []
  , cabal_extrafiles         = []
  , cabal_pkg_config_depends = []
  , cabal_buildType          = Simple
  }

-- import from stdcxx
deletable :: Class
deletable =
  AbstractClass stdcxx_cabal "Deletable" [] mempty Nothing
  [ Destructor Nothing ]
  []
  []


-- import from stdcxx
string :: Class
string =
  Class stdcxx_cabal "string" [ ] mempty
  (Just (ClassAlias { caHaskellName = "CppString", caFFIName = "string"}))
  []
  []
  []
  False

-- -------------------------------------------------------------------
-- OGDF definition
-- -------------------------------------------------------------------

cabal = Cabal {
    cabal_pkgname            = CabalName "OGDF"
  , cabal_version            = "0.5"
  , cabal_cheaderprefix      = "OGDF"
  , cabal_moduleprefix       = "OGDF"
  , cabal_additional_c_incs  = []
  , cabal_additional_c_srcs  = []
  , cabal_additional_pkgdeps = [ CabalName "stdcxx" ]
  , cabal_license            = Just "BSD-3-Clause"
  , cabal_licensefile        = Just "LICENSE"
  , cabal_extraincludedirs   = [ ]
  , cabal_extralibdirs       = []
  , cabal_extrafiles         = []
  , cabal_pkg_config_depends = []
  , cabal_buildType          = Simple
  }

extraLib = [ "OGDF", "COIN" ]

extraDep = [ ]


dPoint :: Class
dPoint =
  Class cabal "DPoint" [deletable] mempty Nothing
  [ Constructor [ double "x", double "y" ] Nothing ]
  []
  []
  False

-- need to be defined with template
dPolyline :: Class
dPolyline =
  Class cabal "DPolyline" [deletable] mempty Nothing
  [
    -- this is incorrect. only for now.
    NonVirtual void_ "pushBack" [ cppclassref dPoint "x" ] Nothing
  ]
  []
  []
  False

edgeElement :: Class
edgeElement =
  Class cabal "EdgeElement" [ deletable ] mempty Nothing
  [ NonVirtual int_ "index" [] Nothing
  , NonVirtual (cppclass_ nodeElement) "source" [] Nothing
  , NonVirtual (cppclass_ nodeElement) "target" [] Nothing
  , NonVirtual self_ "succ" [] Nothing
  , NonVirtual self_ "pred" [] Nothing
  ]
  []
  []
  False

graph :: Class
graph =
  Class cabal "Graph" [ deletable ] mempty Nothing
  [ Constructor [] Nothing
  , NonVirtual (cppclass_ nodeElement) "newNode" [] Nothing
  , NonVirtual (cppclass_ nodeElement) "newNode" [int "index"] (Just "newNode1")
  , NonVirtual (cppclass_ edgeElement) "newEdge" [cppclass nodeElement "v", cppclass nodeElement "w"] Nothing
  , NonVirtual (cppclass_ nodeElement) "firstNode" [] Nothing
  , NonVirtual (cppclass_ nodeElement) "lastNode" [] Nothing
  , NonVirtual (cppclass_ edgeElement) "firstEdge" [] Nothing
  , NonVirtual (cppclass_ edgeElement) "lastEdge" [] Nothing
  ]
  []
  []
  False

graphAttributes :: Class
graphAttributes =
  Class cabal "GraphAttributes" [ deletable ] mempty Nothing
  [ Constructor [ cppclassref graph "g", long "initAttributes" ] Nothing
  , NonVirtual (ref_ CTDouble) "x" [ cppclass nodeElement "v" ] Nothing
  , NonVirtual (ref_ CTDouble) "y" [ cppclass nodeElement "v" ] Nothing
  , NonVirtual (ref_ CTDouble) "width" [ cppclass nodeElement "v" ] Nothing
  , NonVirtual (ref_ CTDouble) "height" [ cppclass nodeElement "v" ] Nothing
  , NonVirtual (cppclassref_ dPolyline) "bends" [ cppclass edgeElement "e" ] Nothing
  , NonVirtual (cppclassref_ string) "label" [ cppclass nodeElement "v" ] Nothing
  , NonVirtual (cppclassref_ string) "label" [ cppclass edgeElement "e" ] (Just "graphAttributeslabelE")
  ]
  []
  []
  False

graphIO :: Class
graphIO =
  Class cabal "GraphIO" [ deletable ] mempty Nothing
  [ Static bool_ "readGML" [ cppclassref graphAttributes "ga", cppclassref graph "g", cppclassref string "filename" ] Nothing
  , Static bool_ "writeGML" [ cppclassref graphAttributes "ga", cppclassref string "filename" ] Nothing
  , Static bool_ "drawSVG" [ cppclassref graphAttributes "ga", cppclassref string "filename" ] Nothing
  ]
  []
  []
  False

hierarchyLayoutModule :: Class
hierarchyLayoutModule =
  Class cabal "HierarchyLayoutModule" [ deletable ] mempty Nothing
  []
  []
  []
  False


layerByLayerSweep :: Class
layerByLayerSweep =
  Class cabal "LayerByLayerSweep" [ deletable, layeredCrossMinModule ] mempty Nothing
  []
  []
  []
  False

layeredCrossMinModule :: Class
layeredCrossMinModule =
  Class cabal "LayeredCrossMinModule" [ deletable ] mempty Nothing
  []
  []
  []
  False

layoutModule :: Class
layoutModule =
  AbstractClass cabal "LayoutModule" [ deletable ] mempty Nothing
  [ Virtual void_ "call" [ cppclassref graphAttributes "ga" ] Nothing
  ]
  []
  []

medianHeuristic :: Class
medianHeuristic =
  Class cabal "MedianHeuristic" [ deletable, layerByLayerSweep ] mempty Nothing
  [ Constructor [] Nothing
  ]
  []
  []
  False

nodeElement :: Class
nodeElement =
  Class cabal "NodeElement" [ deletable ] mempty Nothing
  [ NonVirtual int_ "index" [] Nothing
  , NonVirtual int_ "indeg" [] Nothing
  , NonVirtual int_ "outdeg" [] Nothing
  , NonVirtual int_ "degree" [] Nothing
  , NonVirtual self_ "succ" [] Nothing
  , NonVirtual self_ "pred" [] Nothing
  ]
  []
  []
  False

optimalHierarchyLayout :: Class
optimalHierarchyLayout =
  Class cabal "OptimalHierarchyLayout" [ deletable, hierarchyLayoutModule ] mempty Nothing
  [ Constructor [] Nothing
  , NonVirtual void_ "layerDistance" [ double "x" ] Nothing
  , NonVirtual void_ "nodeDistance" [ double "x" ] Nothing
  , NonVirtual void_ "weightBalancing" [ double "w" ] Nothing
  ]
  []
  []
  False

optimalRanking :: Class
optimalRanking =
  Class cabal "OptimalRanking" [ deletable, rankingModule ] mempty Nothing
  [ Constructor [] Nothing
  ]
  []
  []
  False

rankingModule :: Class
rankingModule =
  Class cabal "RankingModule" [ deletable ] mempty Nothing
  []
  []
  []
  False

sugiyamaLayout :: Class
sugiyamaLayout =
  Class cabal "SugiyamaLayout" [ deletable, layoutModule ] mempty Nothing
  [ Constructor [] Nothing
  , NonVirtual void_ "setCrossMin" [cppclass layeredCrossMinModule "pCrossMin"] Nothing
  , NonVirtual void_ "setLayout" [cppclass hierarchyLayoutModule "pLayout"] Nothing
  , NonVirtual void_ "setRanking" [cppclass rankingModule "pRanking"] Nothing
  ]
  []
  []
  False


classes = [ dPoint
          , dPolyline
          , edgeElement
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

headers =
  [ modImports "DPoint"          ["ogdf"] ["ogdf/basic/geometry.h"]
  , modImports "DPolyline"       ["ogdf"] ["ogdf/basic/geometry.h"]
  , modImports "EdgeElement"     ["ogdf"] ["ogdf/basic/Graph_d.h" ]
  , modImports "Graph"           ["ogdf"] ["ogdf/basic/Graph_d.h" ]
  , modImports "GraphAttributes" ["ogdf"] ["ogdf/basic/GraphAttributes.h"]
  , modImports "GraphIO"         ["ogdf"] ["ogdf/fileformats/GraphIO.h"]
  , modImports "HierarchyLayoutModule" ["ogdf"] ["ogdf/module/HierarchyLayoutModule.h"]
  , modImports "LayerByLayerSweep" ["ogdf"] ["ogdf/module/LayerByLayerSweep.h"]
  , modImports "LayeredCrossMinModule" ["ogdf"] ["ogdf/module/LayeredCrossMinModule.h"]
  , modImports "LayoutModule"    ["ogdf"] ["ogdf/module/LayoutModule.h"]
  , modImports "MedianHeuristic" ["ogdf"] ["ogdf/layered/MedianHeuristic.h"]
  , modImports "NodeElement"     ["ogdf"] ["ogdf/basic/Graph_d.h" ]
  , modImports "OptimalHierarchyLayout" ["ogdf"] ["ogdf/layered/OptimalHierarchyLayout.h"]
  , modImports "OptimalRanking"  ["ogdf"] ["ogdf/layered/OptimalRanking.h"]
  , modImports "RankingModule"   ["ogdf"] ["ogdf/module/RankingModule.h"]
  , modImports "SugiyamaLayout"  ["ogdf"] ["ogdf/layered/SugiyamaLayout.h"]
  ]

main :: IO ()
main = do
  args <- getArgs
  let tmpldir =  if length args == 1
                 then args !! 0
                 else "../template"

  cwd <- getCurrentDirectory
  let fficfg = FFICXXConfig {
                 fficxxconfig_workingDir     = cwd </> "tmp" </> "working"
               , fficxxconfig_installBaseDir = cwd </> "OGDF"
               , fficxxconfig_staticFileDir  = tmpldir
               }
      sbcfg  = SimpleBuilderConfig {
                 sbcTopModule  = "OGDF"
               , sbcModUnitMap = ModuleUnitMap (HM.fromList headers)
               , sbcCabal      = cabal
               , sbcClasses    = classes
               , sbcTopLevels  = toplevelfunctions
               , sbcTemplates  = templates
               , sbcExtraLibs  = extraLib
               , sbcCxxOpts = ["-std=c++17"]
               , sbcExtraDeps  = extraDep
               , sbcStaticFiles = ["LICENSE"]
               }

  simpleBuilder fficfg sbcfg
