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


polygonsToCells :: GeoPolygon -> Int -> Word32 -> Either H3ErrorCodes [H3Index]
polygonsToCells poly res = toEither . hsPolygonToCells poly res

cellsToLinkedMultiPolygon :: [H3Index] -> Either H3ErrorCodes [GeoPolygon]
cellsToLinkedMultiPolygon = toEither . hsCellsToLinkedMultiPolygon 

