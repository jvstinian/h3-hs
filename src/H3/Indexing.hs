module H3.Indexing
  ( LatLng(LatLng)
  , H3ErrorCodes(..)
  , latLngToCell
  , cellToLatLng
  , cellToBoundary
  ) where

import H3.Internal.H3Api 
  ( LatLng(LatLng)
  , H3ErrorCodes(..)
  , H3Index
  , c2hs_latLngToCell
  , c2hs_cellToLatLng
  , c2hs_cellToBoundary )
import H3.Internal.Utils (toEither)

-- |Indexes the location at the specified resolution, returning the index of the cell containing the location. 
--  This buckets the geographic point into the H3 grid. 
--  Note that we are directly binding to the C method, which expects Latitude and Longitude in radians. 
--  This differs from the python bindings which expect the coordinates in degrees and perform the necessary conversion 
--  for the user.
latLngToCell :: LatLng -> Int -> Either H3ErrorCodes H3Index
latLngToCell coords = toEither . c2hs_latLngToCell coords

-- |Finds the center of the cell in grid space. 
cellToLatLng :: H3Index -> Either H3ErrorCodes LatLng
cellToLatLng = toEither . c2hs_cellToLatLng

-- |Finds the boundary of the cell, returning a list of coordinates.
cellToBoundary :: H3Index -> Either H3ErrorCodes [LatLng] 
cellToBoundary = toEither . c2hs_cellToBoundary

