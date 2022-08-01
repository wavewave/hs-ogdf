{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (bracket)
import Control.Monad (void, when)
import Control.Monad.Loops (iterateUntilM)
import Data.Bits ((.|.))
import Data.Foldable (forM_)
import qualified Data.Text.IO as TIO
import Foreign.C.String (withCString)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Formatting ((%),(%.))
import qualified Formatting as F
import OGDF.DPoint
import OGDF.DPolyline
import OGDF.EdgeElement
import OGDF.Graph
import OGDF.GraphAttributes
import OGDF.GraphIO
import OGDF.LayoutModule
import OGDF.MedianHeuristic
import OGDF.NodeElement
import OGDF.OptimalHierarchyLayout
import OGDF.OptimalRanking
import OGDF.SugiyamaLayout
import STD.CppString
import STD.Deletable

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

newGA :: Graph -> IO GraphAttributes
newGA g = newGraphAttributes g (nodeGraphics .|. edgeGraphics .|. nodeLabel)

main :: IO ()
main = do
  bracket newGraph delete $ \g ->
    bracket (newGA g) delete $ \ga -> do
      forM_ [1 .. len-1] $ \i -> do
        left <- graph_newNode g
        p_width1 <- graphAttributes_width ga left
        poke p_width1 (fromIntegral (10*(i+1)))
        p_height1 <- graphAttributes_height ga left
        poke p_height1 15

        bottom <- graph_newNode g
        p_width2 <- graphAttributes_width ga bottom
        poke p_width2 15
        p_height2 <- graphAttributes_height ga bottom
        poke p_height2 (fromIntegral (10*(len+1-i)))

        e <- graph_newEdge g left bottom
        pure ()

      bracket newSugiyamaLayout delete $ \sl -> do
        or <- newOptimalRanking
        sugiyamaLayout_setRanking sl or
        mh <- newMedianHeuristic
        sugiyamaLayout_setCrossMin sl mh
        ohl <- newOptimalHierarchyLayout
        optimalHierarchyLayout_layerDistance ohl 30.0
        optimalHierarchyLayout_nodeDistance ohl 25.0
        optimalHierarchyLayout_weightBalancing ohl 0.8
        sugiyamaLayout_setLayout sl ohl
        call sl ga

      withCString "manual_graph.gml" $ \cstr -> do
        str <- newCppString cstr
        graphIO_write ga str
        delete str

      withCString "manual_graph.svg" $ \cstr -> do
        str <- newCppString cstr
        graphIO_drawSVG ga str
        delete str

      n0@(NodeElement n0') <- graph_firstNode g

      when (n0' /= nullPtr) $ void $
        flip (iterateUntilM (\(NodeElement n'') -> n'' == nullPtr)) n0 $ \n -> do
          i <- nodeElement_index n
          x <- peek =<< graphAttributes_x      ga n
          y <- peek =<< graphAttributes_y      ga n
          w <- peek =<< graphAttributes_width  ga n
          h <- peek =<< graphAttributes_height ga n
          let int = F.left 3 ' ' %. F.int
              dbl = F.left 6 ' ' %. F.float
              txt = F.sformat (int % " " % dbl % " " % dbl % " " % dbl % " " % dbl)
                      (fromIntegral i :: Int)
                      (realToFrac x :: Double)
                      (realToFrac y :: Double)
                      (realToFrac w :: Double)
                      (realToFrac h :: Double)
          TIO.putStrLn txt
          nodeElement_succ n

