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
  , hsPolygonToCells 
  , hsGetRes0Cells
  , hsGetPentagons
  ) where

-- import System.IO.Unsafe (unsafePerformIO)
import Data.Int (Int64)
import Data.Word (Word32)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.Types (CInt, CLong)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(peek))
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (allocaArray, peekArray, callocArray)
import H3.Internal.H3Api 
  ( H3Index
  , H3Error
  , CGeoPolygon
  , newCGeoPolygonPtr 
  , destroyCGeoPolygonPtr 
  , GeoPolygon
  )


-- |degsToRads converts from degrees to radians.
foreign import capi "h3/h3api.h degsToRads" degsToRads :: Double -> Double

-- |radsToDegs converts from radians to degrees
foreign import capi "h3/h3api.h radsToDegs" radsToDegs :: Double -> Double

-- Inspection

foreign import capi "h3/h3api.h getResolution" getResolution :: H3Index -> Int

foreign import capi "h3/h3api.h getBaseCellNumber" getBaseCellNumber :: H3Index -> Int

foreign import capi "h3/h3api.h isValidCell" isValidCell :: H3Index -> Int

foreign import capi "h3/h3api.h isResClassIII" isResClassIII :: H3Index -> Int

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

-- Regions

foreign import capi "h3/h3api.h maxPolygonToCellsSize" cMaxPolygonToCellsSize :: Ptr CGeoPolygon -> Int -> Word32 -> Ptr CLong -> IO H3Error

{-
hsMaxPolygonToCellsSize :: GeoPolygon -> Int -> Word32 -> IO (H3Error, CLong)
hsMaxPolygonToCellsSize poly res flags = do
  cpolyPtr <- newCGeoPolygonPtr poly
  out <- alloca $ \resultPtr -> do
    h3error <- cMaxPolygonToCellsSize cpolyPtr res flags resultPtr
    result <- peek resultPtr
    return (h3error, result)
  destroyCGeoPolygonPtr cpolyPtr 
  return out
-}

foreign import capi "h3/h3api.h polygonToCells" cPolygonToCells :: Ptr CGeoPolygon -> Int -> Word32 -> Ptr H3Index -> IO H3Error

hsPolygonToCellsIO :: GeoPolygon -> Int -> Word32 -> IO (H3Error, [H3Index])
hsPolygonToCellsIO poly res flags = do
  cpolyPtr <- newCGeoPolygonPtr poly
  (h3error, size) <- alloca $ \resultPtr -> do
    h3error <- cMaxPolygonToCellsSize cpolyPtr res flags resultPtr
    result <- peek resultPtr
    return (h3error, result)
  out <- if h3error == 0
         then do let sizei = fromIntegral size
                 resultPtr <- callocArray sizei
                 h3error2 <- cPolygonToCells cpolyPtr res flags resultPtr
                 result <- peekArray sizei resultPtr
                 free resultPtr
                 return (h3error2, result)
         else 
           return (h3error, [])
  destroyCGeoPolygonPtr cpolyPtr 
  return out

hsPolygonToCells :: GeoPolygon -> Int -> Word32 -> (H3Error, [H3Index])
hsPolygonToCells poly res flags = unsafePerformIO $ hsPolygonToCellsIO poly res flags


-- Miscellaneous


foreign import capi "h3/h3api.h res0CellCount" cRes0CellCount :: IO Int

foreign import capi "h3/h3api.h getRes0Cells" cGetRes0Cells :: Ptr H3Index -> IO H3Error

hsGetRes0Cells :: IO (H3Error, [H3Index])
hsGetRes0Cells = do
  cellCount <- cRes0CellCount
  allocaArray cellCount $ \resultPtr -> do
    h3error <- cGetRes0Cells resultPtr
    result <- peekArray cellCount resultPtr
    return (h3error, result)

foreign import capi "h3/h3api.h pentagonCount" cPentagonCount :: IO Int

foreign import capi "h3/h3api.h getPentagons" cGetPentagons :: Int -> Ptr H3Index -> IO H3Error

hsGetPentagons :: Int -> IO (H3Error, [H3Index])
hsGetPentagons res = do
  cellCount <- cPentagonCount
  allocaArray cellCount $ \resultPtr -> do
    h3error <- cGetPentagons res resultPtr
    result <- peekArray cellCount resultPtr
    return (h3error, result)

