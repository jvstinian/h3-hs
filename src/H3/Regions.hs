module H3.Regions
  ( polygonsToCells
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

-- | polygonToCells takes a given GeoJSON-like GeoPolygon data structure and fills it with the hexagons that are contained in the GeoPolygon.  
--   Containment is determined by the cells' centroids.
polygonsToCells :: GeoPolygon -> Int -> Word32 -> Either H3ErrorCodes [H3Index]
polygonsToCells poly res = toEither . hsPolygonToCells poly res

-- | Creates GeoPolygon describing the outline(s) of a set of hexagons. Polygon outlines will follow GeoJSON MultiPolygon order: 
--   Each polygon will have one outer loop, which is first in the list, followed by any holes.  
--   It is expected that all hexagons in the set have the same resolution and that the set contains no duplicates. 
--   Behavior is undefined if duplicates or multiple resolutions are present, and the algorithm may produce unexpected or invalid output.
cellsToLinkedMultiPolygon :: [H3Index] -> Either H3ErrorCodes [GeoPolygon]
cellsToLinkedMultiPolygon = toEither . hsCellsToLinkedMultiPolygon 

