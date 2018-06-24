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


classes = [ graph ]

toplevelfunctions = [ ] 
    
templates = [  ]

headerMap = [ ("Graph", ([NS "ogdf"], [HdrName "ogdf/basic/Graph_d.h"]))
            ]

main :: IO ()
main = do 
  simpleBuilder "OGDF" headerMap (cabal,cabalattr,classes,toplevelfunctions,templates) [ "OGDF" ] extraDep
