{-# LANGUAGE OverloadedStrings #-}

module OGDFIDL where

import qualified Data.HashMap.Strict as HM
--
import FFICXX.Generate.Builder (simpleBuilder)
import FFICXX.Generate.Code.Primitive
  ( bool,
    bool_,
    charpp,
    cppclass,
    cppclass_,
    cppclassref,
    cppclassref_,
    cppclasscopy_,
    cstring,
    cstring_,
    double,
    double_,
    int,
    int_,
    long,
    ref_,
    self_,
    uint,
    uint_,
    void_,
    voidp,
  )
import FFICXX.Generate.Config
  ( FFICXXConfig (..),
    SimpleBuilderConfig (..),
  )
import FFICXX.Generate.Type.Cabal (BuildType (..), Cabal (..), CabalName (..))
import FFICXX.Generate.Type.Class
  ( Arg (..),
    CTypes (CTDouble),
    Class (..),
    ClassAlias (..),
    Form (..),
    Function (..),
    OpExp (OpStar),
    ProtectedMethod (..),
    TemplateAppInfo (..),
    TemplateArgType (..),
    TemplateClass (..),
    TemplateFunction (..),
    TopLevel (..),
    Types (TemplateApp, TemplateAppRef, TemplateParam),
    Variable (..),
  )
import FFICXX.Generate.Type.Config
  ( ModuleUnit (..),
    ModuleUnitImports (..),
    ModuleUnitMap (..),
    modImports,
  )
import FFICXX.Generate.Type.Module (TemplateClassImportHeader (..))
import FFICXX.Runtime.Types (FFISafety (..))
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))

import Data.Foldable (for_)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)

-- -------------------------------------------------------------------
-- import from stdcxx
-- -------------------------------------------------------------------

-- import from stdcxx
stdcxx_cabal =
  Cabal
    { cabal_pkgname = CabalName "stdcxx",
      cabal_version = "0.7.0.1",
      cabal_cheaderprefix = "STD",
      cabal_moduleprefix = "STD",
      cabal_additional_c_incs = [],
      cabal_additional_c_srcs = [],
      cabal_additional_pkgdeps = [],
      cabal_license = Nothing,
      cabal_licensefile = Nothing,
      cabal_extraincludedirs = [],
      cabal_extralibdirs = [],
      cabal_extrafiles = [],
      cabal_pkg_config_depends = [],
      cabal_buildType = Simple
    }

-- import from stdcxx
deletable :: Class
deletable =
  AbstractClass
    stdcxx_cabal
    "Deletable"
    []
    mempty
    Nothing
    [Destructor Nothing]
    []
    []

-- import from stdcxx
string :: Class
string =
  Class
    stdcxx_cabal
    "string"
    []
    mempty
    (Just (ClassAlias {caHaskellName = "CppString", caFFIName = "string"}))
    []
    []
    []
    False

-- -------------------------------------------------------------------
-- OGDF definition
-- -------------------------------------------------------------------

cabal =
  Cabal
    { cabal_pkgname = CabalName "OGDF",
      cabal_version = "1.0.0.1",
      cabal_cheaderprefix = "OGDF",
      cabal_moduleprefix = "OGDF",
      cabal_additional_c_incs = [],
      cabal_additional_c_srcs = [],
      cabal_additional_pkgdeps = [CabalName "stdcxx"],
      cabal_license = Just "BSD-2-Clause",
      cabal_licensefile = Just "LICENSE",
      cabal_extraincludedirs = [],
      cabal_extralibdirs = [],
      cabal_extrafiles = [],
      cabal_pkg_config_depends = [],
      cabal_buildType = Simple
    }

extraLib = ["OGDF", "COIN"]

extraDep = []

--
-- template classes
--

t_List :: TemplateClass
t_List =
  TmplCls
    cabal
    "List"
    (FormSimple "ogdf::List")
    ["tp1"]
    [ TFun
        FFIUnsafe
        (
          TemplateApp
            TemplateAppInfo
              { tapp_tclass = t_ListIterator,
                tapp_tparams = [TArg_TypeParam "tp1"],
                tapp_CppTypeForParam = "ListIterator<tp1>"
              }
        )
        "begin"
        "begin"
        [],
      TFun
        FFIUnsafe
        (
          TemplateApp
            TemplateAppInfo
              { tapp_tclass = t_ListIterator,
                tapp_tparams = [TArg_TypeParam "tp1"],
                tapp_CppTypeForParam = "ListIterator<tp1>"
              }
        )
        "end"
        "end"
        [],
      TFun
        FFIUnsafe
        ( TemplateAppRef
            TemplateAppInfo
              { tapp_tclass = t_ListIterator,
                tapp_tparams = [TArg_TypeParam "tp1"],
                tapp_CppTypeForParam = "ListIterator<tp1>"
              }
        )
        "pushBack"
        "pushBack"
        [Arg (TemplateParam "tp1") "x"]
    ]
    []

t_ListIterator :: TemplateClass
t_ListIterator =
  TmplCls
    cabal
    "ListIterator"
    (FormSimple "ListIterator")
    ["tp1"]
    [ TFunOp
        { tfun_safety = FFIUnsafe,
          tfun_ret = TemplateParam "tp1",
          tfun_name = "deRef",
          tfun_opexp = OpStar
        },
      TFun
        FFIUnsafe
        ( -- TODO: this should be handled with self
          TemplateApp
            TemplateAppInfo
              { tapp_tclass = t_ListIterator,
                tapp_tparams = [TArg_TypeParam "tp1"],
                tapp_CppTypeForParam = "ListIterator<tp1>"
              }
        )
        "listIteratorPred" -- to avoid conflict with Prelude.pred
        "pred"
        [],
      TFun
        FFIUnsafe
        ( -- TODO: this should be handled with self
          TemplateApp
            TemplateAppInfo
              { tapp_tclass = t_ListIterator,
                tapp_tparams = [TArg_TypeParam "tp1"],
                tapp_CppTypeForParam = "ListIterator<tp1>"
              }
        )
        "listIteratorSucc" -- to avoid conflict with Prelude.succ
        "succ"
        [],
      TFun FFIUnsafe bool_ "valid" "valid" []
    ]
    []

--
-- ordinary classes
--

color :: Class
color =
  Class
    cabal
    "Color"
    [deletable]
    mempty
    Nothing
    [ Constructor [cppclassref string "str"] Nothing,
      NonVirtual FFIUnsafe bool_ "fromString" [cppclassref string "str"] Nothing
    ]
    []
    []
    False

dPoint :: Class
dPoint =
  Class
    cabal
    "DPoint"
    [deletable]
    mempty
    Nothing
    [Constructor [double "x", double "y"] Nothing]
    [ Variable (double "m_x"),
      Variable (double "m_y")
    ]
    []
    False

-- need for redefining this with template
dRect :: Class
dRect =
  Class
    cabal
    "DRect"
    [deletable]
    mempty
    Nothing
    [ NonVirtual FFIUnsafe double_ "height" [] Nothing,
      NonVirtual FFIUnsafe double_ "width" [] Nothing
    ]
    []
    []
    False

edgeElement :: Class
edgeElement =
  Class
    cabal
    "EdgeElement"
    [deletable]
    mempty
    Nothing
    [ NonVirtual FFIUnsafe int_ "index" [] Nothing,
      NonVirtual FFIUnsafe (cppclass_ nodeElement) "source" [] Nothing,
      NonVirtual FFIUnsafe (cppclass_ nodeElement) "target" [] Nothing,
      NonVirtual FFIUnsafe self_ "succ" [] Nothing,
      NonVirtual FFIUnsafe self_ "pred" [] Nothing
    ]
    []
    []
    False

fastSimpleHierarchyLayout :: Class
fastSimpleHierarchyLayout =
  Class
    cabal
    "FastSimpleHierarchyLayout"
    [deletable, hierarchyLayoutModule]
    mempty
    Nothing
    [ Constructor [] Nothing,
      NonVirtual FFIUnsafe void_ "layerDistance" [double "x"] Nothing,
      NonVirtual FFIUnsafe void_ "nodeDistance" [double "x"] Nothing,
      NonVirtual FFIUnsafe void_ "downward" [bool "d"] Nothing,
      NonVirtual FFIUnsafe void_ "leftToRight" [bool "b"] Nothing,
      NonVirtual FFIUnsafe void_ "balanced" [bool "b"] Nothing
    ]
    []
    []
    False

fMMMLayout :: Class
fMMMLayout =
  Class
    cabal
    "FMMMLayout"
    [deletable, layoutModule]
    mempty
    Nothing
    [ Constructor [] Nothing,
      NonVirtual FFIUnsafe void_ "useHighLevelOptions" [bool "uho"] Nothing,
      NonVirtual FFIUnsafe void_ "unitEdgeLength" [double "x"] Nothing,
      NonVirtual FFIUnsafe void_ "newInitialPlacement" [bool "nip"] Nothing
      -- NonVirtual void_ "qualityVersusSpeed" [int "qvs"] Nothing -- qvs is FMMMOptions::QualityVsSpeed
    ]
    []
    []
    False

graph :: Class
graph =
  Class
    cabal
    "Graph"
    [deletable]
    mempty
    Nothing
    [ Constructor [] Nothing,
      NonVirtual FFIUnsafe (cppclass_ nodeElement) "newNode" [] Nothing,
      NonVirtual FFIUnsafe (cppclass_ nodeElement) "newNode" [int "index"] (Just "newNode1"),
      NonVirtual FFIUnsafe (cppclass_ edgeElement) "newEdge" [cppclass nodeElement "v", cppclass nodeElement "w"] Nothing,
      NonVirtual FFIUnsafe (cppclass_ nodeElement) "firstNode" [] Nothing,
      NonVirtual FFIUnsafe (cppclass_ nodeElement) "lastNode" [] Nothing,
      NonVirtual FFIUnsafe (cppclass_ edgeElement) "firstEdge" [] Nothing,
      NonVirtual FFIUnsafe (cppclass_ edgeElement) "lastEdge" [] Nothing
    ]
    []
    []
    False

graphAttributes :: Class
graphAttributes =
  let dPolylineRef_ = TemplateAppRef appinfo
        where appinfo =
                TemplateAppInfo
                  { tapp_tclass = t_List
                  , tapp_tparams = [TArg_Class dPoint]
                  , tapp_CppTypeForParam = "List<DPoint>"
                  }
  in Class
       cabal
       "GraphAttributes"
       [deletable]
       mempty
       Nothing
       [ Constructor [cppclassref graph "g", long "initAttributes"] Nothing,
         NonVirtual FFIUnsafe (cppclassref_ color) "fillColor" [cppclass nodeElement "v"] Nothing,
         NonVirtual FFIUnsafe (cppclassref_ color) "fillBgColor" [cppclass nodeElement "v"] Nothing,
         NonVirtual FFIUnsafe (ref_ CTDouble) "x" [cppclass nodeElement "v"] Nothing,
         NonVirtual FFIUnsafe (ref_ CTDouble) "y" [cppclass nodeElement "v"] Nothing,
         NonVirtual FFIUnsafe (ref_ CTDouble) "width" [cppclass nodeElement "v"] Nothing,
         NonVirtual FFIUnsafe (ref_ CTDouble) "height" [cppclass nodeElement "v"] Nothing,
         NonVirtual FFIUnsafe dPolylineRef_ "bends" [cppclass edgeElement "e"] Nothing,
         NonVirtual FFIUnsafe (cppclassref_ string) "label" [cppclass nodeElement "v"] Nothing,
         NonVirtual FFIUnsafe (cppclassref_ string) "label" [cppclass edgeElement "e"] (Just "graphAttributeslabelE"),
         NonVirtual FFIUnsafe (ref_ CTDouble) "xLabel" [cppclass nodeElement "v"] Nothing,
         NonVirtual FFIUnsafe (ref_ CTDouble) "yLabel" [cppclass nodeElement "v"] Nothing,
         Virtual FFIUnsafe (cppclasscopy_ dRect) "boundingBox" [] Nothing
       ]
       []
       []
       False

graphIO :: Class
graphIO =
  Class
    cabal
    "GraphIO"
    [deletable]
    mempty
    Nothing
    [ Static FFIUnsafe bool_ "read" [cppclassref graphAttributes "ga", cppclassref graph "g", cppclassref string "filename"] Nothing,
      Static FFIUnsafe bool_ "write" [cppclassref graphAttributes "ga", cppclassref string "filename"] Nothing,
      Static FFIUnsafe bool_ "drawSVG" [cppclassref graphAttributes "ga", cppclassref string "filename"] Nothing
    ]
    []
    []
    False

hierarchyLayoutModule :: Class
hierarchyLayoutModule =
  Class
    cabal
    "HierarchyLayoutModule"
    [deletable]
    mempty
    Nothing
    []
    []
    []
    False

layerByLayerSweep :: Class
layerByLayerSweep =
  Class
    cabal
    "LayerByLayerSweep"
    [deletable, layeredCrossMinModule]
    mempty
    Nothing
    []
    []
    []
    False

layeredCrossMinModule :: Class
layeredCrossMinModule =
  Class
    cabal
    "LayeredCrossMinModule"
    [deletable]
    mempty
    Nothing
    []
    []
    []
    False

layoutModule :: Class
layoutModule =
  AbstractClass
    cabal
    "LayoutModule"
    [deletable]
    mempty
    Nothing
    [ Virtual FFIUnsafe void_ "call" [cppclassref graphAttributes "ga"] Nothing
    ]
    []
    []

medianHeuristic :: Class
medianHeuristic =
  Class
    cabal
    "MedianHeuristic"
    [deletable, layerByLayerSweep]
    mempty
    Nothing
    [ Constructor [] Nothing
    ]
    []
    []
    False

nodeElement :: Class
nodeElement =
  Class
    cabal
    "NodeElement"
    [deletable]
    mempty
    Nothing
    [ NonVirtual FFIUnsafe int_ "index" [] Nothing,
      NonVirtual FFIUnsafe int_ "indeg" [] Nothing,
      NonVirtual FFIUnsafe int_ "outdeg" [] Nothing,
      NonVirtual FFIUnsafe int_ "degree" [] Nothing,
      NonVirtual FFIUnsafe self_ "succ" [] Nothing,
      NonVirtual FFIUnsafe self_ "pred" [] Nothing
    ]
    []
    []
    False

optimalHierarchyLayout :: Class
optimalHierarchyLayout =
  Class
    cabal
    "OptimalHierarchyLayout"
    [deletable, hierarchyLayoutModule]
    mempty
    Nothing
    [ Constructor [] Nothing,
      NonVirtual FFIUnsafe void_ "layerDistance" [double "x"] Nothing,
      NonVirtual FFIUnsafe void_ "nodeDistance" [double "x"] Nothing,
      NonVirtual FFIUnsafe void_ "weightBalancing" [double "w"] Nothing
    ]
    []
    []
    False

optimalRanking :: Class
optimalRanking =
  Class
    cabal
    "OptimalRanking"
    [deletable, rankingModule]
    mempty
    Nothing
    [ Constructor [] Nothing
    ]
    []
    []
    False

rankingModule :: Class
rankingModule =
  Class
    cabal
    "RankingModule"
    [deletable]
    mempty
    Nothing
    []
    []
    []
    False

sugiyamaLayout :: Class
sugiyamaLayout =
  Class
    cabal
    "SugiyamaLayout"
    [deletable, layoutModule]
    mempty
    Nothing
    [ Constructor [] Nothing,
      NonVirtual FFIUnsafe void_ "pageRatio" [double "x"] Nothing,
      NonVirtual FFIUnsafe void_ "setCrossMin" [cppclass layeredCrossMinModule "pCrossMin"] Nothing,
      NonVirtual FFIUnsafe void_ "setLayout" [cppclass hierarchyLayoutModule "pLayout"] Nothing,
      NonVirtual FFIUnsafe void_ "setRanking" [cppclass rankingModule "pRanking"] Nothing
    ]
    []
    []
    False

classes =
  [ color,
    dPoint,
    -- dPolyline,
    dRect,
    edgeElement,
    graph,
    graphAttributes,
    graphIO,
    fastSimpleHierarchyLayout,
    fMMMLayout,
    hierarchyLayoutModule,
    layerByLayerSweep,
    layeredCrossMinModule,
    layoutModule,
    medianHeuristic,
    nodeElement,
    optimalHierarchyLayout,
    optimalRanking,
    rankingModule,
    sugiyamaLayout
  ]

enums = []

toplevelfunctions = []

templates :: [TemplateClassImportHeader]
templates =
  [ TCIH t_List ["ogdf/basic/List.h"]
  , TCIH t_ListIterator ["ogdf/basic/List.h"]
  ]


headers =
  [ modImports "Color" ["ogdf"] ["ogdf/basic/graphics.h"],
    modImports "DPoint" ["ogdf"] ["ogdf/basic/geometry.h"],
    modImports "DPolyline" ["ogdf"] ["ogdf/basic/geometry.h"],
    modImports "DRect" ["ogdf"] ["ogdf/basic/geometry.h"],
    modImports "EdgeElement" ["ogdf"] ["ogdf/basic/Graph_d.h"],
    modImports "FastSimpleHierarchyLayout" ["ogdf"] ["ogdf/layered/FastSimpleHierarchyLayout.h"],
    modImports "FMMMLayout" ["ogdf"] ["ogdf/energybased/FMMMLayout.h"],
    modImports "Graph" ["ogdf"] ["ogdf/basic/Graph_d.h"],
    modImports "GraphAttributes" ["ogdf"] ["ogdf/basic/GraphAttributes.h"],
    modImports "GraphIO" ["ogdf"] ["ogdf/fileformats/GraphIO.h"],
    modImports "HierarchyLayoutModule" ["ogdf"] ["ogdf/layered/HierarchyLayoutModule.h"],
    modImports "LayerByLayerSweep" ["ogdf"] ["ogdf/layered/LayerByLayerSweep.h"],
    modImports "LayeredCrossMinModule" ["ogdf"] ["ogdf/layered/LayeredCrossMinModule.h"],
    modImports "LayoutModule" ["ogdf"] ["ogdf/basic/LayoutModule.h"],
    modImports "MedianHeuristic" ["ogdf"] ["ogdf/layered/MedianHeuristic.h"],
    modImports "NodeElement" ["ogdf"] ["ogdf/basic/Graph_d.h"],
    modImports "OptimalHierarchyLayout" ["ogdf"] ["ogdf/layered/OptimalHierarchyLayout.h"],
    modImports "OptimalRanking" ["ogdf"] ["ogdf/layered/OptimalRanking.h"],
    modImports "RankingModule" ["ogdf"] ["ogdf/layered/RankingModule.h"],
    modImports "SugiyamaLayout" ["ogdf"] ["ogdf/layered/SugiyamaLayout.h"]
  ]
