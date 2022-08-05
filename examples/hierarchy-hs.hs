module Main where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Bits ((.|.))
import Foreign.C.String (withCString)
import System.IO (hPutStrLn,stderr)

import STD.CppString
import STD.Deletable (delete)
import OGDF.Graph
import OGDF.GraphAttributes
import OGDF.GraphIO
import OGDF.LayoutModule
import OGDF.MedianHeuristic
import OGDF.OptimalHierarchyLayout
import OGDF.OptimalRanking
import OGDF.SugiyamaLayout

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


newGA :: Graph -> IO GraphAttributes
newGA g = newGraphAttributes g (   nodeGraphics
                                 .|. edgeGraphics
                                 .|. nodeLabel
                                 .|. edgeStyle
                                 .|. nodeStyle
                                 .|. nodeTemplate )
main :: IO ()
main = do
  bracket newGraph delete $ \g ->
    bracket (newGA g) delete $ \ga -> do
      withCString "unix-history.gml" $ \inputFileNameCstr ->
        bracket (newCppString inputFileNameCstr) delete $ \inputFileName -> do
          b <- graphIO_read ga g inputFileName
          if (b == 0)
            then hPutStrLn stderr "Could not load unix-history.gml"
            else
              bracket newSugiyamaLayout delete $ \sl -> do
                or <- newOptimalRanking
                sugiyamaLayout_pageRatio sl 2.0
                sugiyamaLayout_setRanking sl or
                mh <- newMedianHeuristic
                sugiyamaLayout_setCrossMin sl mh
                ohl <- newOptimalHierarchyLayout
                optimalHierarchyLayout_layerDistance ohl 30.0
                optimalHierarchyLayout_nodeDistance ohl 25.0
                optimalHierarchyLayout_weightBalancing ohl 0.8
                sugiyamaLayout_setLayout sl ohl
                call sl ga
                -- GML generation
                withCString "unix-history-layout.gml" $ \outputGMLFileNameCstr ->
                  bracket (newCppString outputGMLFileNameCstr) delete $ \outputGMLFileName ->
                    graphIO_write ga outputGMLFileName
                -- SVG generation
                withCString "unix-history-layout.svg" $ \outputSVGFileNameCstr ->
                  bracket (newCppString outputSVGFileNameCstr) delete $ \outputSVGFileName ->
                    graphIO_drawSVG ga outputSVGFileName
                pure ()
