module H3.Miscellaneous 
  ( degsToRads
  , radsToDegs
  , getRes0Cells
  , getPentagons
  , getHexagonAreaAvgKm2 
  , getHexagonAreaAvgM2 
  , cellAreaRads2
  , cellAreaKm2
  , cellAreaM2
  , getHexagonEdgeLengthAvgKm
  , getHexagonEdgeLengthAvgM
  , edgeLengthRads
  , edgeLengthKm
  , edgeLengthM
  , getNumCells
  , greatCircleDistanceKm
  , greatCircleDistanceM
  , greatCircleDistanceRads
  ) where

import Data.Int (Int64)
import H3.Internal.FFI 
  ( degsToRads
  , radsToDegs
  , hsGetRes0Cells
  , hsGetPentagons )
import H3.Internal.H3Api 
  ( H3ErrorCodes
  , H3Index
  , c2hs_getHexagonAreaAvgKm2 
  , c2hs_getHexagonAreaAvgM2 
  , c2hs_cellAreaRads2
  , c2hs_cellAreaKm2
  , c2hs_cellAreaM2
  , c2hs_getHexagonEdgeLengthAvgKm
  , c2hs_getHexagonEdgeLengthAvgM
  , c2hs_edgeLengthRads
  , c2hs_edgeLengthKm
  , c2hs_edgeLengthM
  , c2hs_getNumCells
  , greatCircleDistanceKm
  , greatCircleDistanceM
  , greatCircleDistanceRads )
import H3.Internal.Utils (toEither)


-- | All the resolution 0 H3 cell indexes. These are the coarsest cells that can be represented 
--   in the H3 system and are the parents of all other cell indexes in the H3 grid system. 
--   The returned indexes correspond with the 122 base cells.
getRes0Cells :: Either H3ErrorCodes [H3Index]
getRes0Cells = toEither hsGetRes0Cells

-- | All the pentagon H3 indexes at the specified resolution.
getPentagons :: Int -> Either H3ErrorCodes [H3Index]
getPentagons = toEither . hsGetPentagons

-- | Average hexagon area in square kilometers at the given resolution. Excludes pentagons.
getHexagonAreaAvgKm2 :: Int -> Either H3ErrorCodes Double
getHexagonAreaAvgKm2 = toEither . c2hs_getHexagonAreaAvgKm2 

-- | Average hexagon area in square meters at the given resolution. Excludes pentagons.
getHexagonAreaAvgM2 :: Int -> Either H3ErrorCodes Double
getHexagonAreaAvgM2 = toEither . c2hs_getHexagonAreaAvgM2 

-- | Exact area of specific cell in square radians.
cellAreaRads2 :: H3Index -> Either H3ErrorCodes Double
cellAreaRads2 = toEither . c2hs_cellAreaRads2

-- | Exact area of specific cell in square kilometers.
cellAreaKm2 :: H3Index -> Either H3ErrorCodes Double
cellAreaKm2 = toEither . c2hs_cellAreaKm2

-- | Exact area of specific cell in square meters.
cellAreaM2 :: H3Index -> Either H3ErrorCodes Double
cellAreaM2 = toEither . c2hs_cellAreaM2

-- | Average hexagon edge length in kilometers at the given resolution. Excludes pentagons.
getHexagonEdgeLengthAvgKm :: Int -> Either H3ErrorCodes Double
getHexagonEdgeLengthAvgKm = toEither . c2hs_getHexagonEdgeLengthAvgKm

-- | Average hexagon edge length in meters at the given resolution. Excludes pentagons.
getHexagonEdgeLengthAvgM :: Int -> Either H3ErrorCodes Double
getHexagonEdgeLengthAvgM = toEither . c2hs_getHexagonEdgeLengthAvgM

-- | Exact edge length of specific unidirectional edge in radians.
edgeLengthRads :: H3Index -> Either H3ErrorCodes Double
edgeLengthRads = toEither . c2hs_edgeLengthRads

-- | Exact edge length of specific unidirectional edge in kilometers.
edgeLengthKm :: H3Index -> Either H3ErrorCodes Double
edgeLengthKm = toEither . c2hs_edgeLengthKm

-- | Exact edge length of specific unidirectional edge in meters.
edgeLengthM :: H3Index -> Either H3ErrorCodes Double
edgeLengthM = toEither . c2hs_edgeLengthM

-- | Number of unique H3 indexes at the given resolution.
getNumCells :: Int -> Either H3ErrorCodes Int64
getNumCells = toEither . c2hs_getNumCells

