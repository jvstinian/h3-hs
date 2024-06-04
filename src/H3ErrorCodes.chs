{-# LANGUAGE ForeignFunctionInterface #-}
module H3ErrorCodes 
  ( H3ErrorCodes(..)
  , H3Error
  , H3Index
  , LatLng(LatLng, lat, lng)
  , LatLngPtr
  , CellBoundary(CellBoundary)
  , c2hs_cellToBoundary
  , c2hs_cellToBoundary2
  , cellBoundaryToLatLngs
  , c2hs_latLngToCell
  , c2hs_cellToLatLng
  , c2hs_h3ToString
  , c2hs_cellsToLinkedMultiPolygon
  , hs_cellsToLinkedMultiPolygon 
  , extractGeoPolygons
  , GeoPolygon(GeoPolygon)
  , CGeoPolygon
  , newCGeoPolygonPtr
  , destroyCGeoPolygonPtr
  ) where

import Control.Monad (liftM2, liftM3)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca, finalizerFree, malloc, free)
import Foreign.Marshal.Array (withArrayLen, peekArray, newArray)
import Foreign.C.String (CString, withCStringLen, peekCString, peekCStringLen)
import Foreign.Storable (Storable(peek, poke))
import Foreign.C.Types (CDouble(CDouble), CULong, CUInt, CInt(CInt), CSize)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, newForeignPtr, newForeignPtr_, FinalizerPtr, addForeignPtrFinalizer, mallocForeignPtr)
import Foreign.Ptr (Ptr, castPtr, nullPtr)

#include "h3/h3api.h"

{# enum H3ErrorCodes {} deriving (Show, Eq) #}

-- type H3Error = {#type uint32_t #}
type H3Error = CUInt -- {#type H3Error #}
type H3Index =  CULong --  {#type uint64_t #}


data LatLng = LatLng 
    { lat :: Double -- CDouble
    , lng :: Double -- CDouble
    }
  deriving (Eq, Show)

{# pointer *LatLng as LatLngPtr -> LatLng #}

instance Storable LatLng where
    sizeOf _ = {# sizeof LatLng #}
    alignment _ = {# alignof LatLng #}
    {-
    peek p = LatLng <$> {# get LatLng->lat #} p
                    <*> {# get LatLng->lng #} p
    -}
    peek p = do
      CDouble _lat <- {# get LatLng->lat #} p
      CDouble _lng <- {# get LatLng->lng #} p
      return $ LatLng _lat _lng
    poke p (LatLng x y) = do
        {# set LatLng->lat #} p (CDouble x)
        {# set LatLng->lng #} p (CDouble y)

maxCellBndryVerts :: Int
maxCellBndryVerts = {# const MAX_CELL_BNDRY_VERTS #}

data CellBoundary = CellBoundary
    { numVerts :: CInt
    , verts :: Ptr LatLng
    }
  deriving (Eq, Show)

instance Storable CellBoundary where
    sizeOf _ = {# sizeof CellBoundary #}
    alignment _ = {# alignof CellBoundary #}
    peek p = CellBoundary <$> {# get CellBoundary->numVerts #} p
                          <*> {# get CellBoundary->verts #} p
    poke p (CellBoundary _num _verts) = do
        {# set CellBoundary->numVerts #} p _num
        {# set CellBoundary->verts #} p _verts

{-
type CellBoundary = [LatLng]

instance Storable CellBoundary where
    sizeOf _ = {# sizeof CellBoundary #}
    alignment _ = {# alignof CellBoundary #}
    peek p = liftM2 peekArray ({# get CellBoundary->numVerts #} p) ({# get CellBoundary->verts #} p)
    poke p latlngs = do
        {# set CellBoundary->numVerts #} p (length latlngs)
        pokeArray ? latlngs 
        {# set CellBoundary->verts #} :: Ptr CellBoundary -> LatLng -> IO ()
-}

{- The following does not work, I get the following error: 
 -    • Couldn't match type ‘CellBoundary’ with ‘()’
 -      Expected: Ptr ()
 -        Actual: Ptr CellBoundary
 -    • In the second argument of ‘c2hs_cellToBoundary'_’, namely ‘a2'’

withPlaceholderCellBoundary :: (Ptr CellBoundary -> IO b) -> IO b
withPlaceholderCellBoundary f =
  withArrayLen (replicate 10 (LatLng 0 0)) $ \num llptr -> do
    alloca $ \cellptr -> do
      poke cellptr (CellBoundary (fromIntegral num) llptr)
      f cellptr

withPlaceholderCellBoundary2 :: (Ptr CellBoundary -> IO b) -> IO b
withPlaceholderCellBoundary2 f =
  withArrayLen (replicate 10 (LatLng 0 0)) $ \num llptr -> do
    with (CellBoundary (fromIntegral num) llptr) f

{#fun pure cellToBoundary as c2hs_cellToBoundary 
      { fromIntegral                 `H3Index', 
        withPlaceholderCellBoundary- `CellBoundary' peek*
      } -> `H3Error' fromIntegral #}

-}

{# pointer *CellBoundary as CellBoundaryPtr -> CellBoundary #}

withPlaceholderCellBoundary2 :: (Ptr CellBoundary -> IO b) -> IO b
withPlaceholderCellBoundary2 f =
  withArrayLen (replicate 10 (LatLng 0 0)) $ \num llptr -> do
    with (CellBoundary (fromIntegral num) llptr) f

{#fun pure cellToBoundary as c2hs_cellToBoundary 
      { fromIntegral                  `H3Index', 
        withPlaceholderCellBoundary2- `CellBoundary' peek* -- id -- peek*
      } -> `H3Error' fromIntegral #}

cellBoundaryToLatLngs :: CellBoundaryPtr {- Ptr CellBoundary -} -> IO [LatLng]
cellBoundaryToLatLngs cellptr = do
      CellBoundary resnum resllptr <- peek cellptr
      resll <- peekArray (fromIntegral resnum) resllptr
      return resll

{#fun pure cellToBoundary as c2hs_cellToBoundary2
      { fromIntegral                  `H3Index', 
        withPlaceholderCellBoundary2- `[LatLng]' cellBoundaryToLatLngs* 
      } -> `H3Error' fromIntegral #}


{#fun pure latLngToCell as c2hs_latLngToCell
      { with*   `LatLng',
                `Int',
        alloca- `H3Index' peek*
      } -> `H3Error' fromIntegral #}

{#fun pure cellToLatLng as c2hs_cellToLatLng
      { fromIntegral `H3Index',
        alloca- `LatLng' peek*
      } -> `H3Error' fromIntegral #}


-- I don't see an allocaCStringLen in Foreign.C.String.
allocaCStringLen :: ((CString, {-CSize-}CULong)-> IO a) -> IO a
allocaCStringLen fn = withCStringLen dummyString fnint
    where dummyString = replicate 17 '0' -- '\NUL'
          fnint (cstr, i) = fn (cstr, fromIntegral i)

peekCStringWithLen :: CString -> CULong -> IO String
peekCStringWithLen = curry (peekCStringLen . ulongConvert)
    where ulongConvert (cstr, ulong) = (cstr, fromIntegral ulong)

peekCStringWithLen2 :: CString -> CULong -> IO String
peekCStringWithLen2 cstr _ = peekCString cstr

{#fun pure h3ToString as c2hs_h3ToString
      { fromIntegral `H3Index',
        allocaCStringLen- `String'& peekCStringWithLen2*
      } -> `H3Error' fromIntegral #}
-- H3Error h3ToString(H3Index h, char *str, size_t sz);

