{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Miscellaneous 
  ( degsToRads
  , hsLatLngToCell
  , h3ToString
  , cellToLatLng
  , cellToBoundary
  , c_getResolution
  , hs_maxPolygonToCellsSize 
  {-, hsLatLngToCell0-}) where

import Control.Monad (liftM2)
import Data.List (replicate)
import Data.Word (Word32)
import Data.Int (Int64)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (withArrayLen, peekArray, callocArray)
-- import Foreign.Marshal.Utils (with)
import Foreign.Storable (Storable(poke, peek))
import Foreign.C.Types (CUInt(..), CULong(..), CSize(..), CChar, CLong) -- To work with H3Error below
import Foreign.C.String (CString, withCString, peekCString)
import H3ErrorCodes (LatLng(LatLng), LatLngPtr, H3Index, H3Error, CellBoundary(CellBoundary), GeoPolygon, CGeoPolygon, newCGeoPolygonPtr, destroyCGeoPolygonPtr)
-- import Foreign.C.ConstPtr (ConstPtr)

-- foreign import ccall "degsToRads" degsToRads :: CDouble -> CDouble
foreign import capi "h3/h3api.h degsToRads" degsToRads :: Double -> Double

foreign import capi "h3/h3api.h latLngToCell" latLngToCell :: LatLngPtr -> Int -> Ptr H3Index -> IO H3Error 

foreign import capi "h3/h3api.h cellToLatLng" cCellToLatLng :: H3Index -> LatLngPtr -> IO H3Error 

{-
hsLatLngToCell0 :: LatLng -> Int -> IO (H3Index, H3Error, H3Index)
hsLatLngToCell0 coords res = do
  alloca $ \coordsPtr -> do
      poke coordsPtr coords
      alloca $ \resultPtr -> do
        result0 <- peek resultPtr
        status <- latLngToCell coordsPtr res resultPtr
        result <- peek resultPtr
        return (result, status, result0)
-}


hsLatLngToCell :: LatLng -> Int -> IO (H3Index, H3Error)
hsLatLngToCell coords res = do
  alloca $ \coordsPtr -> do
      poke coordsPtr coords
      alloca $ \resultPtr -> do
        -- liftM2 (,) (peek resultPtr) (latLngToCell coordsPtr res resultPtr)
        status <- latLngToCell coordsPtr res resultPtr
        result <- peek resultPtr
        return (result, status)

cellToLatLng :: H3Index -> IO (LatLng, H3Error)
cellToLatLng h3index = do
  alloca $ \latLngPtr -> do
    status <- cCellToLatLng h3index latLngPtr
    latlng <- peek latLngPtr
    return (latlng, status)


foreign import capi "h3/h3api.h h3ToString" c_h3ToString :: H3Index -> CString -> CSize -> IO H3Error 

h3ToString :: H3Index -> IO (String, H3Error)
h3ToString h3index = do
  withCString initstr $ \cstr -> do
    status <- c_h3ToString h3index cstr 17 -- NOTE: It is unclear whether the size parameter (17 here) should count the null terminating character
    result <- peekCString cstr
    return (result, status)
  where initstr = replicate 16 '0'  -- NOTE: I only initialize a string with 16 characters.  See note above
  
foreign import capi "h3/h3api.h cellToBoundary" c_cellToBoundary :: H3Index -> Ptr CellBoundary -> IO H3Error 

cellToBoundary :: H3Index -> IO ([LatLng], H3Error)
cellToBoundary h3index = do
  withArrayLen (replicate 10 (LatLng 0 0)) $ \num llptr -> do
    alloca $ \cellptr -> do
      poke cellptr (CellBoundary (fromIntegral num) llptr) 
      status <- c_cellToBoundary h3index cellptr
      CellBoundary resnum resllptr <- peek cellptr
      resll <- peekArray (fromIntegral resnum) resllptr
      return (resll, status)

-- describeH3Error is not currently included in the h3api.h header file
-- foreign import capi "h3/h3api.h describeH3Error" c_describeH3Error :: H3Error -> IO CString

foreign import capi "h3/h3api.h getResolution" c_getResolution :: H3Index -> IO Int

getResolution :: H3Index -> Int
getResolution = unsafePerformIO . c_getResolution

foreign import capi "h3/h3api.h getBaseCellNumber" c_getBaseCellNumber :: H3Index -> IO Int

getBaseCellNumber :: H3Index -> Int
getBaseCellNumber = unsafePerformIO . c_getBaseCellNumber

foreign import capi "h3/h3api.h maxPolygonToCellsSize" c_maxPolygonToCellsSize :: Ptr CGeoPolygon -> Int -> {-CUInt-} Word32 -> Ptr CLong -> IO H3Error

foreign import capi "h3/h3api.h polygonToCells" c_polygonToCells :: Ptr CGeoPolygon -> Int -> {-CUInt-} Word32 -> Ptr H3Index -> IO H3Error

hs_maxPolygonToCellsSize :: GeoPolygon -> Int -> Word32 -> IO (H3Error, CLong)
hs_maxPolygonToCellsSize poly res flags = do
  cpolyPtr <- newCGeoPolygonPtr poly
  out <- alloca $ \resultPtr -> do
    h3error <- c_maxPolygonToCellsSize cpolyPtr res flags resultPtr
    result <- peek resultPtr
    return (h3error, result)
  destroyCGeoPolygonPtr cpolyPtr 
  return out
  {-
  alloca $ \cpolyPtr -> do
    cpoly <- newCGeoPolygon poly
    poke cpolyPtr cpoly
    out <- alloca $ \resultPtr -> do
      h3error <- c_maxPolygonToCellsSize cpolyPtr res flags resultPtr
      result <- peek resultPtr
      return (h3error, result)
    destroyCGeoPolygon cpoly
    return out
  -}

hs_polygonToCells :: GeoPolygon -> Int -> Word32 -> IO (H3Error, [H3Index])
hs_polygonToCells poly res flags = do
  cpolyPtr <- newCGeoPolygonPtr poly
  (h3error, size) <- alloca $ \resultPtr -> do
    h3error <- c_maxPolygonToCellsSize cpolyPtr res flags resultPtr
    result <- peek resultPtr
    return (h3error, result)
  out <- if h3error == 0
         then do let sizei = fromIntegral size
                 resultPtr <- callocArray sizei
                 h3error2 <- c_polygonToCells cpolyPtr res flags resultPtr
                 result <- peekArray sizei resultPtr
                 free resultPtr
                 return (h3error2, result)
         else 
           return (h3error, [])
  destroyCGeoPolygonPtr cpolyPtr 
  return out

