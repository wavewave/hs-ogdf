{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Bits ((.|.))
import Data.Foldable (forM_)
import Foreign.C.String (withCString)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import OGDF.CppString
import OGDF.Deletable
import OGDF.Graph
import OGDF.GraphAttributes
import OGDF.GraphIO
import OGDF.NodeElement

nodeGraphics     = 0x000001
edgeGraphics     = 0x000002
edgeIntWeight    = 0x000004
edgeDoubleWeight = 0x000008
edgeLabel        = 0x000010
nodeLabel        = 0x000020
edgeType         = 0x000040
nodeType         = 0x000080
nodeId           = 0x000100
edgeArrow        = 0x000200
edgeStyle        = 0x000400
nodeStyle        = 0x000800
nodeTemplate     = 0x001000
edgeSubGraphs    = 0x002000
nodeWeight       = 0x004000
threeD           = 0x010000

len = 11

main :: IO ()
main = do
  g <- newGraph
  ga <- newGraphAttributes g (nodeGraphics .|. edgeGraphics)

  forM_ [1 .. len-1] $ \i -> do
    left <- graphnewNode g
    p_x <- graphAttributesx ga left
    poke p_x (fromIntegral (-5*(i+1)))
    p_y <- graphAttributesy ga left
    poke p_y (fromIntegral (-20*i))
    p_width <- graphAttributeswidth ga left
    poke p_width (fromIntegral (10*(i+1)))
    p_height <- graphAttributesheight ga left
    poke p_height (fromIntegral 15)


  withCString "manual_graph.gml" $ \cstr -> do
    str <- newCppString cstr
    graphIOwriteGML ga str
    delete str

  delete ga
  delete g
  pure ()
