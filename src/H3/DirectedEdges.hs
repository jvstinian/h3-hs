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

import H3.Internal.H3Api 
  ( LatLng
  , H3ErrorCodes
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


-- | Returns a list of length 2 consisting of the origin and the destination hexagon IDs for the given edge ID.
directedEdgeToCells :: H3Index -> Either H3ErrorCodes [H3Index]
directedEdgeToCells = toEither . hsDirectedEdgeToCells

-- | Provides all of the directed edges from the current H3Index. 
--   The return will be of length 6, but the number of directed edges placed in the array may be less than 6. 
--   If this is the case, one of the members of the array will be 0.
originToDirectedEdges :: H3Index -> Either H3ErrorCodes [H3Index]
originToDirectedEdges = toEither . hsOriginToDirectedEdges

-- | Returns whether or not the provided H3 cell indexes are neighbors.
areNeighborCells :: H3Index -> H3Index -> Either H3ErrorCodes Bool
areNeighborCells origin = toEither . c2hs_areNeighborCells origin

-- | Returns a unidirectional edge H3 index based on the provided origin and destination.
cellsToDirectedEdge :: H3Index -> H3Index -> Either H3ErrorCodes H3Index
cellsToDirectedEdge origin = toEither . c2hs_cellsToDirectedEdge origin 

-- | Returns the origin hexagon from the unidirectional edge H3Index.
getDirectedEdgeOrigin :: H3Index -> Either H3ErrorCodes H3Index
getDirectedEdgeOrigin = toEither . c2hs_getDirectedEdgeOrigin 

-- | Returns the destination hexagon from the unidirectional edge H3Index.
getDirectedEdgeDestination :: H3Index -> Either H3ErrorCodes H3Index
getDirectedEdgeDestination = toEither . c2hs_getDirectedEdgeDestination

-- | Provides the coordinates defining the unidirectional edge.
directedEdgeToBoundary :: H3Index -> Either H3ErrorCodes [LatLng]
directedEdgeToBoundary = toEither . c2hs_directedEdgeToBoundary

