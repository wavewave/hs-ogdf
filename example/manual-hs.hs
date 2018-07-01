{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Bits ((.|.))
import Data.Foldable (forM_)
import Foreign.C.String (withCString)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

import STD.CppString
import STD.Deletable
import OGDF.DPoint
import OGDF.DPolyline
import OGDF.EdgeElement
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
    p_x1 <- graphAttributesx ga left
    poke p_x1 (fromIntegral (-5*(i+1)))
    p_y1 <- graphAttributesy ga left
    poke p_y1 (fromIntegral (-20*i))
    p_width1 <- graphAttributeswidth ga left
    poke p_width1 (fromIntegral (10*(i+1)))
    p_height1 <- graphAttributesheight ga left
    poke p_height1 15

    bottom <- graphnewNode g
    p_x2 <- graphAttributesx ga bottom
    poke p_x2 (fromIntegral (20*(len-i)))
    p_y2 <- graphAttributesy ga bottom
    poke p_y2 (fromIntegral (5*(len+1-i)))
    p_width2 <- graphAttributeswidth ga bottom
    poke p_width2 15
    p_height2 <- graphAttributesheight ga bottom
    poke p_height2 (fromIntegral (10*(len+1-i)))

    e <- graphnewEdge g left bottom
    poly <- graphAttributesbends ga e
    pt1 <- newDPoint 10 (fromIntegral (-20*i))
    pt2 <- newDPoint (fromIntegral (20*(len-i))) (-10)
    dPolylinepushBack poly pt1
    dPolylinepushBack poly pt2


  withCString "manual_graph.gml" $ \cstr -> do
    str <- newCppString cstr
    graphIOwriteGML ga str
    delete str

  delete ga
  delete g
  pure ()
