{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module H3.Internal.FFI
  ( degsToRads
  ) where

{-
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
-}
foreign import capi "h3/h3api.h degsToRads" degsToRads :: Double -> Double

