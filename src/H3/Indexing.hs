module H3.Indexing
  ( LatLng(LatLng)
  , latLngToCell
  , cellToLatLng
  , cellToBoundary
  ) where

import H3.Internal.H3Api 
  ( LatLng(LatLng)
  , H3ErrorCodes
  , H3Index
  , c2hs_latLngToCell
  , c2hs_cellToLatLng
  , c2hs_cellToBoundary )
import H3.Internal.Utils (toEither)


latLngToCell :: LatLng -> Int -> Either H3ErrorCodes H3Index
latLngToCell coords = toEither . c2hs_latLngToCell coords

cellToLatLng :: H3Index -> Either H3ErrorCodes LatLng
cellToLatLng = toEither . c2hs_cellToLatLng

cellToBoundary :: H3Index -> Either H3ErrorCodes [LatLng] 
cellToBoundary = toEither . c2hs_cellToBoundary

