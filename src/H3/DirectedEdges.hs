module H3.DirectedEdges
  ( isValidDirectedEdge 
  , directedEdgeToCells
  , originToDirectedEdges
  , areNeighborCells
  , cellsToDirectedEdge
  , getDirectedEdgeOrigin
  , getDirectedEdgeDestination
  , directedEdgeToBoundary
  ) where

import H3.Indexing (LatLng)
import H3.Internal.H3Api 
  ( H3ErrorCodes
  , H3Index
  , c2hs_areNeighborCells
  , c2hs_cellsToDirectedEdge
  , c2hs_getDirectedEdgeOrigin
  , c2hs_getDirectedEdgeDestination
  , c2hs_directedEdgeToBoundary
  )
import H3.Internal.FFI 
  ( isValidDirectedEdge
  , hsDirectedEdgeToCells
  , hsOriginToDirectedEdges
  )
import H3.Internal.Utils (toEither)

directedEdgeToCells :: H3Index -> Either H3ErrorCodes [H3Index]
directedEdgeToCells = toEither . hsDirectedEdgeToCells

originToDirectedEdges :: H3Index -> Either H3ErrorCodes [H3Index]
originToDirectedEdges = toEither . hsOriginToDirectedEdges

areNeighborCells :: H3Index -> H3Index -> Either H3ErrorCodes Bool
areNeighborCells origin = toEither . c2hs_areNeighborCells origin

cellsToDirectedEdge :: H3Index -> H3Index -> Either H3ErrorCodes H3Index
cellsToDirectedEdge origin = toEither . c2hs_cellsToDirectedEdge origin 

getDirectedEdgeOrigin :: H3Index -> Either H3ErrorCodes H3Index
getDirectedEdgeOrigin = toEither . c2hs_getDirectedEdgeOrigin 

getDirectedEdgeDestination :: H3Index -> Either H3ErrorCodes H3Index
getDirectedEdgeDestination = toEither . c2hs_getDirectedEdgeDestination

directedEdgeToBoundary :: H3Index -> Either H3ErrorCodes [LatLng]
directedEdgeToBoundary = toEither . c2hs_directedEdgeToBoundary

