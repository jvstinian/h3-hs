module H3.Traversals
  ( gridDisk
  , gridDiskUnsafe
  , gridDiskDistances
  , gridDiskDistancesSafe
  , gridDiskDistancesUnsafe
  , gridRingUnsafe
  , gridPathCells
  , gridDistance 
  , cellToLocalIj
  , localIjToCell
  ) where

import Data.Int (Int64)
import H3.Internal.H3Api 
  ( H3ErrorCodes
  , H3Index
  , c2hs_gridDistance
  , c2hs_cellToLocalIj
  , c2hs_localIjToCell
  , CoordIJ
  )
import H3.Internal.FFI 
  ( hsGridDisk
  , hsGridDiskUnsafe
  , hsGridDiskDistances 
  , hsGridDiskDistancesSafe 
  , hsGridDiskDistancesUnsafe
  , hsGridRingUnsafe
  , hsGridPathCells
  )
import H3.Internal.Utils (toEither)

gridDisk :: H3Index -> Int -> Either H3ErrorCodes [H3Index]
gridDisk origin = toEither . hsGridDisk origin

gridDiskUnsafe :: H3Index -> Int -> Either H3ErrorCodes [H3Index]
gridDiskUnsafe origin = toEither . hsGridDiskUnsafe origin

gridDiskDistances :: H3Index -> Int -> Either H3ErrorCodes ([H3Index], [Int])
gridDiskDistances origin = toEither . hsGridDiskDistances origin 

gridDiskDistancesSafe :: H3Index -> Int -> Either H3ErrorCodes ([H3Index], [Int])
gridDiskDistancesSafe origin = toEither . hsGridDiskDistancesSafe origin 

gridDiskDistancesUnsafe :: H3Index -> Int -> Either H3ErrorCodes ([H3Index], [Int])
gridDiskDistancesUnsafe origin = toEither . hsGridDiskDistancesUnsafe origin 

gridRingUnsafe :: H3Index -> Int -> Either H3ErrorCodes [H3Index]
gridRingUnsafe h3index = toEither . hsGridRingUnsafe h3index 

gridPathCells :: H3Index -> H3Index -> Either H3ErrorCodes [H3Index]
gridPathCells h3index = toEither . hsGridPathCells h3index

-- | Provides the distance in grid cells between the two indexes.
--   Returns an error if finding the distance failed. 
--   Finding the distance can fail because the two indexes are not comparable (different resolutions), 
--   too far apart, 
--   or are separated by pentagonal distortion. 
--   This is the same set of limitations as the local IJ coordinate space functions.
gridDistance :: H3Index -> H3Index -> Either H3ErrorCodes Int64
gridDistance origin = toEither . c2hs_gridDistance origin

-- | Produces local IJ coordinates for an H3 index anchored by an origin.
--   The C API has an additional argument mode which is reserved for future expansion and must be set to 0. 
--   The method provided here automatically passes the value. 
--   This function's output is not guaranteed to be compatible across different versions of H3.
cellToLocalIj :: H3Index -> H3Index -> Either H3ErrorCodes CoordIJ
cellToLocalIj origin h3 = toEither $ c2hs_cellToLocalIj origin h3 0

-- | Produces an H3 index from local IJ coordinates anchored by an origin.
--   The C API has an additional argument mode which is reserved for future expansion and must be set to 0, 
--   and the method defined here automatically passes the value. 
--   This function's output is not guaranteed to be compatible across different versions of H3.
localIjToCell :: H3Index -> CoordIJ -> Either H3ErrorCodes H3Index
localIjToCell origin ij = toEither $ c2hs_localIjToCell origin ij 0

