{-|
Module      : H3.Data

This module collects the data types provided by the H3 API.
-}
module H3.Data
  ( LatLng(LatLng, lat, lng)
  , H3Index
  , H3ErrorCodes(..)
  , GeoLoop
  , GeoPolygon(GeoPolygon)
  , CoordIJ(CoordIJ)
  ) where

import H3.Internal.H3Api 
  ( LatLng(LatLng, lat, lng)
  , H3ErrorCodes(..)
  , H3Index
  , GeoLoop
  , GeoPolygon(GeoPolygon)
  , CoordIJ(CoordIJ)
  )

