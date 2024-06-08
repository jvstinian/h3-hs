module H3.Traversal
  ( CoordIJ(CoordIJ)
  , gridDisk
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
  , CoordIJ(CoordIJ)
  , c2hs_gridDistance
  , c2hs_cellToLocalIj
  , c2hs_localIjToCell
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

-- | gridDisk produces indices within k distance of the origin index.
--   Elements of the output array may be left as zero, which can happen when crossing a pentagon.
gridDisk :: H3Index -> Int -> Either H3ErrorCodes [H3Index]
gridDisk origin = toEither . hsGridDisk origin

-- | gridDiskUnsafe produces indexes within k distance of the origin index. 
--   The function returns an error code when one of the returned by this function is a pentagon 
--   or is in the pentagon distortion area. 
--   In this case, the output behavior of the out array is undefined.
gridDiskUnsafe :: H3Index -> Int -> Either H3ErrorCodes [H3Index]
gridDiskUnsafe origin = toEither . hsGridDiskUnsafe origin

-- | gridDiskDistances produces indices within k distance of the origin index.
--    k-ring 0 is defined as the origin index, k-ring 1 is defined as k-ring 0 
--    and all neighboring indices, and so on.
gridDiskDistances :: H3Index -> Int -> Either H3ErrorCodes ([H3Index], [Int])
gridDiskDistances origin = toEither . hsGridDiskDistances origin 

-- | gridDiskDistancesSafe produces indexes within k distance of the origin index.
gridDiskDistancesSafe :: H3Index -> Int -> Either H3ErrorCodes ([H3Index], [Int])
gridDiskDistancesSafe origin = toEither . hsGridDiskDistancesSafe origin 

-- | gridDiskDistancesUnsafe produces indexes within k distance of the origin index. 
--   Output behavior is undefined when one of the indexes returned by this function is a pentagon 
--   or is in the pentagon distortion area.
gridDiskDistancesUnsafe :: H3Index -> Int -> Either H3ErrorCodes ([H3Index], [Int])
gridDiskDistancesUnsafe origin = toEither . hsGridDiskDistancesUnsafe origin 

-- | Produces the hollow hexagonal ring centered at origin with sides of length k.
gridRingUnsafe :: H3Index -> Int -> Either H3ErrorCodes [H3Index]
gridRingUnsafe h3index = toEither . hsGridRingUnsafe h3index 

-- | Given two H3 indexes, return the line of indexes between them (inclusive).
--   This function may fail to find the line between two indexes, for example if they are very far apart. 
--   It may also fail when finding distances for indexes on opposite sides of a pentagon.
--
--   Notes:
--
--   The specific output of this function should not be considered stable across library versions. 
--   The only guarantees the library provides are that the line length will be consistent with the distance method 
--   and that every index in the line will be a neighbor of the preceding index.
--
--   Lines are drawn in grid space, and may not correspond exactly to either Cartesian lines or great arcs.
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

