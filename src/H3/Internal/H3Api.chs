{-# LANGUAGE ForeignFunctionInterface #-}
module H3.Internal.H3Api
  ( H3ErrorCodes(..)
  , H3Error
  , H3Index
  ) where

import Foreign.C.Types (CULong, CUInt)
{-
import Control.Monad (liftM2, liftM3)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca, finalizerFree, malloc, free)
import Foreign.Marshal.Array (withArrayLen, peekArray, newArray)
import Foreign.C.String (CString, withCStringLen, peekCString, peekCStringLen)
import Foreign.Storable (Storable(peek, poke))
import Foreign.C.Types (CDouble(CDouble), CULong, CUInt, CInt(CInt), CSize)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, newForeignPtr, newForeignPtr_, FinalizerPtr, addForeignPtrFinalizer, mallocForeignPtr)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
-}

#include "h3/h3api.h"

-- |The C H3 error codes are modeled as a Haskell data type and instance of Enum
{# enum H3ErrorCodes {} deriving (Show, Eq) #}

-- |H3Error is the output for most of the funtions in the H3 API. 
--  The C type is uint32_t, in Haskell we use CUInt.
type H3Error = CUInt

-- |H3Index is the numeric representation of the H3 geohashing.  
--  The C type is uint64_t, in Haskell we represent this as CULong.
type H3Index =  CULong

