{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Exception (bracket)
import Control.Monad (void)
import Control.Monad.Extra (ifM, loopM)
import Data.Bits ((.|.))
import FFICXX.Runtime.TH (IsCPrimitive (..), TemplateParamInfo (..))
import Foreign.C.String (withCString)
import Foreign.C.Types (CBool (..))
import Foreign.Ptr (nullPtr)
import Foreign.Storable (peek)
import System.IO (hPutStrLn,stderr)

import STD.CppString
import STD.Deletable (delete)
import OGDF.DPoint
import OGDF.DPoint.Implementation (dPoint_m_x_get, dPoint_m_y_get)
import OGDF.EdgeElement
import OGDF.Graph
import OGDF.GraphAttributes
import OGDF.GraphIO
import OGDF.LayoutModule
import qualified OGDF.List.TH as TH
import OGDF.List.Template
import qualified OGDF.ListIterator.TH as TH
import OGDF.ListIterator.Template
import OGDF.MedianHeuristic
import OGDF.NodeElement
import OGDF.OptimalHierarchyLayout
import OGDF.OptimalRanking
import OGDF.SugiyamaLayout

TH.genListInstanceFor
  NonCPrim
  ( [t|DPoint|],
    TPInfo
      { tpinfoCxxType = "DPoint",
        tpinfoCxxHeaders = ["ogdf/basic/geometry.h", "OGDFType.h"],
        tpinfoCxxNamespaces = ["ogdf"],
        tpinfoSuffix = "DPoint"
      }
  )

TH.genListIteratorInstanceFor
  NonCPrim
  ( [t|DPoint|],
    TPInfo
      { tpinfoCxxType = "DPoint",
        tpinfoCxxHeaders = ["ogdf/basic/geometry.h", "OGDFType.h"],
        tpinfoCxxNamespaces = ["ogdf"],
        tpinfoSuffix = "DPoint"
      }
  )

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
threeD           = 0x008000


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
                -- extracting node coordinates
                putStrLn "#### Node layout information ####"
                n0@(NodeElement _) <- graph_firstNode g
                flip loopM n0 $ \n@(NodeElement nPtr) ->
                  if nPtr == nullPtr
                    then pure (Right ())
                    else do
                      j :: Int <- fromIntegral <$> nodeElement_index n
                      x :: Double <- realToFrac <$> (peek =<< graphAttributes_x ga n)
                      y :: Double <- realToFrac <$> (peek =<< graphAttributes_y ga n)
                      w :: Double <- realToFrac <$> (peek =<< graphAttributes_width ga n)
                      h :: Double <- realToFrac <$> (peek =<< graphAttributes_height ga n)
                      print (j, x, y, w, h)
                      Left <$> nodeElement_succ n
                -- extracting edge information
                putStrLn "#### Edge layout information ####"
                e0@(EdgeElement _) <- graph_firstEdge g
                flip loopM e0 $ \e@(EdgeElement ePtr) ->
                  if ePtr == nullPtr
                    then pure (Right ())
                    else do
                      j :: Int <- fromIntegral <$> edgeElement_index e
                      dpline <- graphAttributes_bends ga e
                      print j
                      it0 <- begin dpline
                      flip loopM (0, it0) $ \(i, it) -> do
                        ifM ((/= 0) <$> valid it)
                          ( do
                              putStrLn ("A" ++ show i)
                              p <- deRef it
                              x <- dPoint_m_x_get p
                              y <- dPoint_m_y_get p
                              print (x, y)
                              (Left . (i+1,) <$> listIteratorSucc it)
                          )
                          (print "B" >> pure (Right ()))
                      Left <$> edgeElement_succ e
                pure ()
