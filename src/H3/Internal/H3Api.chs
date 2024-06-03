{-# LANGUAGE ForeignFunctionInterface #-}
module H3.Internal.H3Api
  ( H3ErrorCodes(..)
  , H3Error
  , H3Index
  , LatLng(LatLng)
  , c2hs_latLngToCell
  , c2hs_cellToLatLng
  , c2hs_cellToBoundary
  , c2hs_h3ToString
  , c2hs_stringToH3
  ) where

import Foreign.C.Types (CULong, CInt, CDouble(CDouble))
import Data.Word (Word64, Word32)
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (withArrayLen, peekArray)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.String (CString, withCStringLen, peekCString, withCString)
import Foreign.Storable (Storable(peek, poke))


#include "h3/h3api.h"

-- |The C H3 error codes are modeled as a Haskell data type and instance of Enum
{# enum H3ErrorCodes {} deriving (Show, Eq) #}

-- |H3Error is the output for most of the funtions in the H3 API. 
--  The C type is uint32_t, in Haskell we use CUInt.
type H3Error = Word32

-- |H3Index is a type synonym for Word64, which we use as 
--  the numeric representation of the H3 index in Haskell
type H3Index = Word64

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

peekAsH3Index :: Ptr CULong -> IO Word64
peekAsH3Index ptr = fromIntegral <$> peek ptr

{#fun pure latLngToCell as c2hs_latLngToCell
      { with*   `LatLng',
                `Int',
        alloca- `H3Index' peekAsH3Index*
      } -> `H3Error' fromIntegral #}

{#fun pure cellToLatLng as c2hs_cellToLatLng
      { fromIntegral `H3Index',
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
      { fromIntegral                  `H3Index', 
        withPlaceholderCellBoundary-  `[LatLng]' cellBoundaryToLatLngs* 
      } -> `H3Error' fromIntegral #}

-- Inspection Methods

allocaCStringLen :: ((CString, CULong)-> IO a) -> IO a
allocaCStringLen fn = withCStringLen dummyString fnint
    where dummyString = replicate 17 '0'
          fnint (cstr, i) = fn (cstr, fromIntegral i)

peekCStringIgnoreLen :: CString -> CULong -> IO String
peekCStringIgnoreLen cstr _ = peekCString cstr

{#fun pure h3ToString as c2hs_h3ToString
      { fromIntegral `H3Index',
        allocaCStringLen- `String'& peekCStringIgnoreLen*
      } -> `H3Error' fromIntegral #}

{#fun pure stringToH3 as c2hs_stringToH3
      { withCString* `String',
        alloca- `H3Index' peekAsH3Index*
      } -> `H3Error' fromIntegral #}

