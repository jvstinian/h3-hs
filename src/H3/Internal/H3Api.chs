{-# LANGUAGE ForeignFunctionInterface #-}
module H3.Internal.H3Api
  ( H3ErrorCodes(..)
  , H3Error
  , CH3Index
  , LatLng(LatLng)
  , c2hs_latLngToCell
  , c2hs_cellToLatLng
  , c2hs_cellToBoundary
  ) where

import Foreign.C.Types (CULong, CUInt, CInt, CDouble(CDouble))
-- import Data.Word (Word64, Word32)
import Foreign.Ptr (Ptr{-, castPtr, nullPtr-})
import Foreign.Marshal.Array (withArrayLen, peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(peek, poke))


#include "h3/h3api.h"

-- |The C H3 error codes are modeled as a Haskell data type and instance of Enum
{# enum H3ErrorCodes {} deriving (Show, Eq) #}

-- |H3Error is the output for most of the funtions in the H3 API. 
--  The C type is uint32_t, in Haskell we use CUInt.
type H3Error = CUInt -- Word32 seems to work as well

-- |CH3Index is the numeric representation of the H3 geohashing in C.
--  The C type is uint64_t, in Haskell we represent this as CULong.
type CH3Index = CULong --Word64

data LatLng = LatLng 
    { lat :: Double -- ^ Latitude
    , lng :: Double -- ^ Longitude
    }
  deriving (Eq, Show)

instance Storable LatLng where
    sizeOf _ = {# sizeof LatLng #}
    alignment _ = {# alignof LatLng #}
    peek p = do
      CDouble _lat <- {# get LatLng->lat #} p
      CDouble _lng <- {# get LatLng->lng #} p
      return $ LatLng _lat _lng
    poke p (LatLng x y) = do
        {# set LatLng->lat #} p (CDouble x)
        {# set LatLng->lng #} p (CDouble y)

-- |LatLngPtr which is needed for the c2hs fun hooks
{# pointer *LatLng as LatLngPtr -> LatLng #}

{-
peekH3Index :: Ptr CULong -> IO Word64
peekH3Index ptr = fromIntegral <$> peek ptr
-}

-- {# pointer *uint64_t as CH3IndexPtr -> CH3Index #}

{#fun pure latLngToCell as c2hs_latLngToCell
      { with*   `LatLng',
                `Int',
        alloca- `CH3Index' peek*
      } -> `H3Error' fromIntegral #}

{#fun pure cellToLatLng as c2hs_cellToLatLng
      { fromIntegral `CH3Index',
        alloca- `LatLng' peek*
      } -> `H3Error' fromIntegral #}

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

{# pointer *CellBoundary as CellBoundaryPtr -> CellBoundary #}

withPlaceholderCellBoundary :: (Ptr CellBoundary -> IO b) -> IO b
withPlaceholderCellBoundary f =
  let dummyBoundary = replicate maxCellBndryVerts (LatLng 0 0)
  in withArrayLen dummyBoundary $ \num llptr -> do
       with (CellBoundary (fromIntegral num) llptr) f

cellBoundaryToLatLngs :: CellBoundaryPtr -> IO [LatLng]
cellBoundaryToLatLngs cellptr = do
      CellBoundary resnum resllptr <- peek cellptr
      resll <- peekArray (fromIntegral resnum) resllptr
      return resll

{#fun pure cellToBoundary as c2hs_cellToBoundary
      { fromIntegral                  `CH3Index', 
        withPlaceholderCellBoundary-  `[LatLng]' cellBoundaryToLatLngs* 
      } -> `H3Error' fromIntegral #}

