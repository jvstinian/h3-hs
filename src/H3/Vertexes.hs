{-|
Module      : H3.Vertexes

Vertex mode allows encoding the topological vertexes of H3 cells.
-}
module H3.Vertexes
  ( cellToVertex
  , cellToVertexes 
  , vertexToLatLng
  , isValidVertex
  ) where

import H3.Internal.H3Api 
  ( LatLng
  , H3ErrorCodes
  , H3Index
  , c2hs_cellToVertex
  , c2hs_vertexToLatLng
  )
import H3.Internal.FFI 
  ( hsCellToVertexes 
  , isValidVertex
  )
import H3.Internal.Utils (toEither)

-- | Returns the index for the specified cell vertex and vertex number. 
--   Valid vertex numbers are between 0 and 5 (inclusive) for hexagonal cells, 
--   and 0 and 4 (inclusive) for pentagonal cells.
cellToVertex :: H3Index -- ^ origin
             -> Int     -- ^ vertexNum
             -> Either H3ErrorCodes H3Index
cellToVertex origin = toEither . c2hs_cellToVertex origin 

-- | Returns the latitude and longitude coordinates of the given vertex.
vertexToLatLng :: H3Index -> Either H3ErrorCodes LatLng
vertexToLatLng = toEither . c2hs_vertexToLatLng 

-- | Returns the indexes for all vertexes of the given cell index.
--   The length of the returned list is 6. 
--   If the given cell index represents a pentagon, 
--   one member of the list will be set to 0.
cellToVertexes :: H3Index -> Either H3ErrorCodes [H3Index]
cellToVertexes = toEither . hsCellToVertexes

