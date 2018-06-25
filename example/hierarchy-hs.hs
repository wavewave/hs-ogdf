module Main where

import Data.Bits ((.|.))
import Foreign.C.String (newCString)
import System.IO (hPutStrLn,stderr)

import OGDF.Graph
import OGDF.GraphAttributes
import OGDF.GraphIO
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
  b <- graphIOreadGML g cstr

  if (b == 0)
    then hPutStrLn stderr "Could not load unix-history.gml"
    else do
      sl <- newSugiyamaLayout
      putStrLn "sl created"
      or <- newOptimalRanking
      putStrLn "or created"
      sugiyamaLayoutsetRanking sl or
      putStrLn "setRanking done"
