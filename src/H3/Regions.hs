module H3.Regions
  ( polygonToCells
  , cellsToLinkedMultiPolygon
  ) where

import Data.Word (Word32)
import H3.Internal.H3Api 
  ( H3ErrorCodes
  , H3Index
  , GeoPolygon
  , hsCellsToLinkedMultiPolygon 
  )
import H3.Internal.FFI 
  ( hsPolygonToCells 
  )
import H3.Internal.Utils (toEither)

-- | @polygonToCells@ takes a given GeoJSON-like 'GeoPolygon' data structure and fills it with the hexagons that are contained in the 'GeoPolygon'.  
--   Containment is determined by the cells' centroids. 
--   An argument for @flags@ is provided, which is reserved for future functionality, and should be taken to be 0 here.
polygonToCells :: GeoPolygon -- ^ geoPolygon
               -> Int        -- ^ res
               -> Word32     -- ^ flags
               -> Either H3ErrorCodes [H3Index]
polygonToCells poly res = toEither . hsPolygonToCells poly res

-- | Creates 'GeoPolygon' describing the outline(s) of a set of hexagons. 
--   Polygon outlines will have one outer loop and a list of loops representing holes.  
--   It is expected that all hexagons in the set have the same resolution and that the set contains no duplicates. 
--   Behavior is undefined if duplicates or multiple resolutions are present, and the algorithm may produce unexpected or invalid output.
cellsToLinkedMultiPolygon :: [H3Index] -> Either H3ErrorCodes [GeoPolygon]
cellsToLinkedMultiPolygon = toEither . hsCellsToLinkedMultiPolygon 

