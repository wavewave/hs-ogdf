module Main where

import Data.Bits ((.|.))
import Foreign.C.String (newCString)
import System.IO (hPutStrLn,stderr)

import OGDF.CppString
import OGDF.Deletable (delete)
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


main :: IO ()
main = do
  g <- newGraph
  putStrLn "g created"
  ga <- newGraphAttributes g (   nodeGraphics
                             .|. edgeGraphics
                             .|. nodeLabel
                             .|. edgeStyle
                             .|. nodeStyle
                             .|. nodeTemplate )
  putStrLn "ga created"

  cstr <- newCString "example/unix-history.gml"
  str <- newCppString cstr
  b <- graphIOreadGML ga g str

  if (b == 0)
    then hPutStrLn stderr "Could not load unix-history.gml"
    else do
      sl <- newSugiyamaLayout
      putStrLn "sl created"
      or <- newOptimalRanking
      putStrLn "or created"
      sugiyamaLayoutsetRanking sl or
      putStrLn "setRanking done"
      mh <- newMedianHeuristic
      putStrLn "mh created"
      sugiyamaLayoutsetCrossMin sl mh
      putStrLn "setCrossMin"

      ohl <- newOptimalHierarchyLayout
      putStrLn "ohl created"
      optimalHierarchyLayoutlayerDistance ohl 30.0
      optimalHierarchyLayoutnodeDistance ohl 25.0
      optimalHierarchyLayoutweightBalancing ohl 0.8
      sugiyamaLayoutsetLayout sl ohl
      putStrLn "setLayout ohl"
      call sl ga
      putStrLn "SL.call(GA)"
      cstrout <- newCString "unix-history-layout.gml"
      strout <- newCppString cstrout
      graphIOwriteGML ga strout
      delete strout
      delete sl
      pure ()

  delete g
  delete ga
  delete str
