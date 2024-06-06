{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module H3.Internal.FFI
  ( degsToRads
  , radsToDegs
  , getResolution
  , getBaseCellNumber
  , isValidCell
  , isResClassIII
  , isPentagon
  , hsGetIcosahedronFaces 
  ) where

import H3.Internal.H3Api (H3Index, H3Error)
import Foreign.C.Types (CInt)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(peek))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)


-- |degsToRads converts from degrees to radians.
foreign import capi "h3/h3api.h degsToRads" degsToRads :: Double -> Double

-- |radsToDegs converts from radians to degrees
foreign import capi "h3/h3api.h radsToDegs" radsToDegs :: Double -> Double

-- Inspection

-- | Returns the resolution of the index.
foreign import capi "h3/h3api.h getResolution" getResolution :: H3Index -> Int

-- | Returns the base cell number of the index.
foreign import capi "h3/h3api.h getBaseCellNumber" getBaseCellNumber :: H3Index -> Int

-- | isValidCell returns non-zero if this is a valid H3 cell index
foreign import capi "h3/h3api.h isValidCell" isValidCell :: H3Index -> Int

-- | Returns non-zero if this index has a resolution with Class III orientation.
foreign import capi "h3/h3api.h isResClassIII" isResClassIII :: H3Index -> Int

-- | Returns non-zero if this index represents a pentagonal cell.
foreign import capi "h3/h3api.h isPentagon" isPentagon :: H3Index -> Int

foreign import capi "h3/h3api.h maxFaceCount" c_maxFaceCount :: H3Index -> Ptr CInt -> IO H3Error

foreign import capi "h3/h3api.h getIcosahedronFaces" c_getIcosahedronFaces :: H3Index -> Ptr CInt -> IO H3Error

hsGetIcosahedronFaces :: H3Index -> (H3Error, [Int])
hsGetIcosahedronFaces h3index = 
  unsafePerformIO $ alloca $ \countptr -> do
    h3error <- c_maxFaceCount h3index countptr
    if h3error == 0
    then do
      count <- fromIntegral <$> peek countptr
      allocaArray count $ \facesptr -> do
        out3error <- c_getIcosahedronFaces h3index facesptr
        if out3error == 0
        then do
          faces <- map fromIntegral <$> peekArray count facesptr
          return (out3error, faces)
        else return (out3error, [])
    else return (h3error, [])

