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
import FFICXX.Generate.Type.Config    ( ModuleUnit(..), ModuleUnitMap(..), ModuleUnitImports(..) )
import FFICXX.Generate.Type.Class     ( Class(..)
                                      , ClassAlias(..)
                                      , CTypes(CTDouble)
                                      , Function(..)
                                      , ProtectedMethod(..)
                                      , TopLevelFunction(..)
                                      , Variable(..)
                                      )
import FFICXX.Generate.Type.Config    ( ModuleUnit(..)
                                      , ModuleUnitImports(..)
                                      )
import FFICXX.Generate.Type.PackageInterface ( Namespace(..), HeaderName(..) )


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
  , cabal_license            = Just "BSD3"
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

graphIO :: Class
graphIO =
  Class cabal "GraphIO" [ deletable ] mempty Nothing
  [ Static bool_ "readGML" [ cppclassref graphAttributes "ga", cppclassref graph "g", cppclassref string "filename" ] Nothing
  , Static bool_ "writeGML" [ cppclassref graphAttributes "ga", cppclassref string "filename" ] Nothing
  , Static bool_ "drawSVG" [ cppclassref graphAttributes "ga", cppclassref string "filename" ] Nothing
  ]
  []
  []


hierarchyLayoutModule :: Class
hierarchyLayoutModule =
  Class cabal "HierarchyLayoutModule" [ deletable ] mempty Nothing
  []
  []
  []



layerByLayerSweep :: Class
layerByLayerSweep =
  Class cabal "LayerByLayerSweep" [ deletable, layeredCrossMinModule ] mempty Nothing
  []
  []
  []

layeredCrossMinModule :: Class
layeredCrossMinModule =
  Class cabal "LayeredCrossMinModule" [ deletable ] mempty Nothing
  []
  []
  []

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

optimalRanking :: Class
optimalRanking =
  Class cabal "OptimalRanking" [ deletable, rankingModule ] mempty Nothing
  [ Constructor [] Nothing
  ]
  []
  []

rankingModule :: Class
rankingModule =
  Class cabal "RankingModule" [ deletable ] mempty Nothing
  []
  []
  []

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
  [ ( MU_Class "DPoint"
    , ModuleUnitImports {
        muimports_namespaces = [ NS "ogdf" ]
      , muimports_headers    = [ HdrName "ogdf/basic/geometry.h" ]
      }
    )
  , ( MU_Class "DPolyline"
    , ModuleUnitImports {
        muimports_namespaces = [ NS "ogdf" ]
      , muimports_headers    = [ HdrName "ogdf/basic/geometry.h" ]
      }
    )
  , ( MU_Class "EdgeElement"
    , ModuleUnitImports {
        muimports_namespaces = [ NS "ogdf" ]
      , muimports_headers    = [ HdrName "ogdf/basic/Graph_d.h" ]
      }
    )
  , ( MU_Class "Graph"
    , ModuleUnitImports {
        muimports_namespaces = [ NS "ogdf" ]
      , muimports_headers    = [ HdrName "ogdf/basic/Graph_d.h" ]
      }
    )
  , ( MU_Class "GraphAttributes"
    , ModuleUnitImports {
        muimports_namespaces = [ NS "ogdf" ]
      , muimports_headers    = [ HdrName "ogdf/basic/GraphAttributes.h" ]
      }
    )
  , ( MU_Class "GraphIO"
    , ModuleUnitImports {
        muimports_namespaces = [ NS "ogdf" ]
      , muimports_headers    = [ HdrName "ogdf/fileformats/GraphIO.h" ]
      }
    )
  , ( MU_Class "HierarchyLayoutModule"
    , ModuleUnitImports {
        muimports_namespaces = [ NS "ogdf" ]
      , muimports_headers    = [ HdrName "ogdf/module/HierarchyLayoutModule.h" ]
      }
    )
  , ( MU_Class "LayerByLayerSweep"
    , ModuleUnitImports {
        muimports_namespaces = [ NS "ogdf" ]
      , muimports_headers    = [ HdrName "ogdf/module/LayerByLayerSweep.h" ]
      }
    )
  , ( MU_Class "LayeredCrossMinModule"
    , ModuleUnitImports {
        muimports_namespaces = [ NS "ogdf" ]
      , muimports_headers    = [ HdrName "ogdf/module/LayeredCrossMinModule.h" ]
      }
    )
  , ( MU_Class "LayoutModule"
    , ModuleUnitImports {
        muimports_namespaces = [ NS "ogdf" ]
      , muimports_headers    = [ HdrName "ogdf/module/LayoutModule.h" ]
      }
    )
  , ( MU_Class "MedianHeuristic"
    , ModuleUnitImports {
        muimports_namespaces = [ NS "ogdf" ]
      , muimports_headers    = [ HdrName "ogdf/layered/MedianHeuristic.h" ]
      }
    )
  , ( MU_Class "NodeElement"
    , ModuleUnitImports {
        muimports_namespaces = [ NS "ogdf" ]
      , muimports_headers    = [ HdrName "ogdf/basic/Graph_d.h" ]
      }
    )
  , ( MU_Class "OptimalHierarchyLayout"
    , ModuleUnitImports {
        muimports_namespaces = [ NS "ogdf" ]
      , muimports_headers    = [ HdrName "ogdf/layered/OptimalHierarchyLayout.h" ]
      }
    )
  , ( MU_Class "OptimalRanking"
    , ModuleUnitImports {
        muimports_namespaces = [ NS "ogdf" ]
      , muimports_headers    = [ HdrName "ogdf/layered/OptimalRanking.h" ]
      }
    )
  , ( MU_Class "RankingModule"
    , ModuleUnitImports {
        muimports_namespaces = [ NS "ogdf" ]
      , muimports_headers    = [ HdrName "ogdf/module/RankingModule.h" ]
      }
    )
  , ( MU_Class "SugiyamaLayout"
    , ModuleUnitImports {
        muimports_namespaces = [ NS "ogdf" ]
      , muimports_headers    = [ HdrName "ogdf/layered/SugiyamaLayout.h" ]
      }
    )
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
               , fficxxconfig_installBaseDir = cwd </> "hgdal"
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
               , sbcExtraDeps  = extraDep
               , sbcStaticFiles = ["LICENSE"]
               }

  simpleBuilder fficfg sbcfg