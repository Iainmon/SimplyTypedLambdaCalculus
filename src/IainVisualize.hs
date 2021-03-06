module IainVisualize (
  printGraph,
  toDirectedDot,
  plotDGraphPng
) where

import Control.Concurrent

import           Data.GraphViz
import           Data.GraphViz.Types
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Attributes.Colors
import           Data.GraphViz.Attributes.Colors.Brewer
import Data.GraphViz.Printing
import           Data.Hashable
import qualified Data.Text.Lazy                    as TL

import Data.Graph.DGraph
import Data.Graph.Types
import Data.Graph.UGraph

-- | Plot an undirected 'UGraph'
plotUGraph :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => UGraph v e
 -> IO ThreadId
plotUGraph g = forkIO $ runGraphvizCanvas Sfdp (toUndirectedDot False g) Xlib

-- | Same as 'plotUGraph' but render edge attributes
plotUGraphEdged :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => UGraph v e
 -> IO ThreadId
plotUGraphEdged g = forkIO $ runGraphvizCanvas Sfdp (toUndirectedDot True g) Xlib

-- | Plot an undirected 'UGraph' to a PNG image file
plotUGraphPng :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => UGraph v e
 -> FilePath
 -> IO FilePath
plotUGraphPng g = addExtension (runGraphvizCommand Sfdp $ toUndirectedDot False g) Png

-- | Plot a directed 'DGraph'
plotDGraph :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => DGraph v e
 -> IO ThreadId
plotDGraph g = forkIO $ runGraphvizCanvas Sfdp (toDirectedDot False g) Xlib

-- | Same as 'plotDGraph' but render edge attributes
plotDGraphEdged :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => DGraph v e
 -> IO ThreadId
plotDGraphEdged g = forkIO $ runGraphvizCanvas Sfdp (toDirectedDot True g) Xlib




command = Dot -- Sfdp

-- | Plot a directed 'DGraph' to a PNG image file
plotDGraphPng :: (Hashable v, Ord v, PrintDot v, Show v, Show e)
 => DGraph v e
 -> FilePath
 -> IO FilePath
plotDGraphPng g = addExtension (runGraphvizCommand command $ toDirectedDot False g) Png



labeledNodes :: (Graph g, Show v) => g v e -> [(v, String)]
labeledNodes g = (\v -> (v, show v)) <$> vertices g

labeledEdges :: (Hashable v, Eq v, Show e) => UGraph v e -> [(v, v, String)]
labeledEdges g = (\(Edge v1 v2 attr) -> (v1, v2, show attr)) <$> edges g

labeledArcs :: (Hashable v, Eq v, Show e) => DGraph v e -> [(v, v, String)]
labeledArcs g = (\(Arc v1 v2 attr) -> (v1, v2, show attr)) <$> arcs g

toUndirectedDot :: (Hashable v, Ord v, Show v, Show e)
 => Bool -- ^ Label edges
 -> UGraph v e
 -> DotGraph v
toUndirectedDot labelEdges g = graphElemsToDot params (labeledNodes g) (labeledEdges g)
    where params = sensibleDotParams False labelEdges

toDirectedDot :: (Hashable v, Ord v, Show v, Show e)
 => Bool -- ^ Label edges
 -> DGraph v e
 -> DotGraph v
toDirectedDot labelEdges g = graphElemsToDot params (labeledNodes g) (labeledArcs g)
    where params = sensibleDotParams True labelEdges

printGraph :: PrintDot g => g -> String
printGraph = TL.unpack . renderDot . toDot -- . graphToDot . nonClusteredParams

sensibleDotParams
 :: Bool -- ^ Directed
 -> Bool -- ^ Label edges
 -> GraphvizParams t l String () l
sensibleDotParams directed edgeLabeled = nonClusteredParams
    { isDirected = directed
    , globalAttributes =
        [ GraphAttrs [
           Overlap ScaleOverlaps
          -- , Layout Patchwork
          ]
        , NodeAttrs [
             Shape Circle
            , Style [SItem Filled []]
            , FillColor [(toWC (X11Color Orange))] 
            -- , BgColor (toWC $ toColor (Brewer (BScheme Spectral 4)))
            -- , BgColor [(toWC (X11Color Orange))]
            , Color [(toWC (X11Color Gray))]
          ]
        , EdgeAttrs [
             FontColor (X11Color DarkGreen)
            , ArrowHead (AType [(noMods,NoArrow)])
          ]
        ]
    , fmtEdge = edgeFmt
    }
    where
        edgeFmt (_, _, l) = if edgeLabeled
            then [Label $ StrLabel $ TL.pack l]
            else []