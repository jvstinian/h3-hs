module H3.Vertexes
  ( cellToVertex
  , cellToVertexes 
  , vertexToLatLng
  , isValidVertex
  ) where

import H3.Indexing (LatLng)
import H3.Internal.H3Api 
  ( H3ErrorCodes
  , H3Index
  , c2hs_cellToVertex
  , c2hs_vertexToLatLng
  )
import H3.Internal.FFI 
  ( hsCellToVertexes 
  , isValidVertex
  )
import H3.Internal.Utils (toEither)

cellToVertex :: H3Index -> Int -> Either H3ErrorCodes H3Index
cellToVertex origin = toEither . c2hs_cellToVertex origin 

vertexToLatLng :: H3Index -> Either H3ErrorCodes LatLng
vertexToLatLng = toEither . c2hs_vertexToLatLng 

cellToVertexes :: H3Index -> Either H3ErrorCodes [H3Index]
cellToVertexes = toEither . hsCellToVertexes

