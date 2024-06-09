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
  , hsGridDisk
  , hsGridDiskUnsafe
  , hsGridDiskDistances 
  , hsGridDiskDistancesSafe 
  , hsGridDiskDistancesUnsafe
  , hsGridRingUnsafe
  , hsGridPathCells
  , hsCellToChildren
  , hsCompactCells 
  , hsUncompactCells 
  , hsUncompactCellsUsingSize 
  , isValidDirectedEdge
  , hsDirectedEdgeToCells
  , hsOriginToDirectedEdges
  ) where

import Data.Int (Int64)
import Data.Word (Word32)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.Types (CInt, CLong)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(peek))
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (allocaArray, withArray, peekArray, callocArray, withArray, withArrayLen)
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


-- Regions


foreign import capi "h3/h3api.h maxPolygonToCellsSize" cMaxPolygonToCellsSize :: Ptr CGeoPolygon -> Int -> Word32 -> Ptr CLong -> IO H3Error

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

hsGetRes0Cells :: (H3Error, [H3Index])
hsGetRes0Cells = unsafePerformIO $ do
  cellCount <- cRes0CellCount
  allocaArray cellCount $ \resultPtr -> do
    h3error <- cGetRes0Cells resultPtr
    result <- peekArray cellCount resultPtr
    return (h3error, result)

foreign import capi "h3/h3api.h pentagonCount" cPentagonCount :: IO Int

foreign import capi "h3/h3api.h getPentagons" cGetPentagons :: Int -> Ptr H3Index -> IO H3Error

hsGetPentagons :: Int -> (H3Error, [H3Index])
hsGetPentagons res = unsafePerformIO $ do
  cellCount <- cPentagonCount
  allocaArray cellCount $ \resultPtr -> do
    h3error <- cGetPentagons res resultPtr
    result <- peekArray cellCount resultPtr
    return (h3error, result)


-- Traversals

foreign import capi "h3/h3api.h maxGridDiskSize" cMaxGridDiskSize :: Int -> Ptr Int64 -> IO H3Error

hsGridDiskUsingMethod :: (H3Index -> Int -> Ptr H3Index -> IO H3Error) -> H3Index -> Int -> IO (H3Error, [H3Index])
hsGridDiskUsingMethod diskMethod h3index k = do
  alloca $ \maxSizePtr -> do
    sizeh3error <- cMaxGridDiskSize k maxSizePtr
    if sizeh3error == 0
    then do
      maxSize <- fromIntegral <$> peek maxSizePtr
      -- TODO: Perhaps set up a custom gridRingUnsafe method and revert the following back to allocaArray maxSize $ ...
      withArray (replicate maxSize 0) $ \resultPtr -> do
        h3error <- diskMethod h3index k resultPtr
        result <- peekArray maxSize resultPtr
        return (h3error, result)
    else do
      return (sizeh3error, [])
 
foreign import capi "h3/h3api.h gridDisk" cGridDisk :: H3Index -> Int -> Ptr H3Index -> IO H3Error

hsGridDisk :: H3Index -> Int -> (H3Error, [H3Index])
hsGridDisk origin = unsafePerformIO . hsGridDiskUsingMethod cGridDisk origin

foreign import capi "h3/h3api.h gridDiskUnsafe" cGridDiskUnsafe :: H3Index -> Int -> Ptr H3Index -> IO H3Error

hsGridDiskUnsafe :: H3Index -> Int -> (H3Error, [H3Index])
hsGridDiskUnsafe origin = unsafePerformIO . hsGridDiskUsingMethod cGridDiskUnsafe origin

hsGridDiskDistancesUsingMethod :: (H3Index -> Int -> Ptr H3Index -> Ptr CInt -> IO H3Error) -> H3Index -> Int -> IO (H3Error, ([H3Index], [Int]))
hsGridDiskDistancesUsingMethod diskDistanceMethod h3index k = do
  alloca $ \maxSizePtr -> do
    sizeh3error <- cMaxGridDiskSize k maxSizePtr
    if sizeh3error == 0
    then do
      maxSize <- fromIntegral <$> peek maxSizePtr
      allocaArray maxSize $ \indexResultPtr -> do
        allocaArray maxSize $ \distanceResultPtr -> do
          h3error <- diskDistanceMethod h3index k indexResultPtr distanceResultPtr
          indexResult <- peekArray maxSize indexResultPtr
          distanceResult <- map fromIntegral <$> peekArray maxSize distanceResultPtr
          return (h3error, (indexResult, distanceResult))
    else do
      return (sizeh3error, ([], []))

foreign import capi "h3/h3api.h gridDiskDistances" cGridDiskDistances :: H3Index -> Int -> Ptr H3Index -> Ptr CInt -> IO H3Error

hsGridDiskDistances :: H3Index -> Int -> (H3Error, ([H3Index], [Int]))
hsGridDiskDistances origin = unsafePerformIO . hsGridDiskDistancesUsingMethod cGridDiskDistances origin

foreign import capi "h3/h3api.h gridDiskDistancesSafe" cGridDiskDistancesSafe :: H3Index -> Int -> Ptr H3Index -> Ptr CInt -> IO H3Error

hsGridDiskDistancesSafe :: H3Index -> Int -> (H3Error, ([H3Index], [Int]))
hsGridDiskDistancesSafe origin = unsafePerformIO . hsGridDiskDistancesUsingMethod cGridDiskDistancesSafe origin

foreign import capi "h3/h3api.h gridDiskDistancesUnsafe" cGridDiskDistancesUnsafe :: H3Index -> Int -> Ptr H3Index -> Ptr CInt -> IO H3Error

hsGridDiskDistancesUnsafe :: H3Index -> Int -> (H3Error, ([H3Index], [Int]))
hsGridDiskDistancesUnsafe origin = unsafePerformIO . hsGridDiskDistancesUsingMethod cGridDiskDistancesUnsafe origin

-- We assume the gridRingUnsafe method also expects an array of size maxGridDiskSize
foreign import capi "h3/h3api.h gridRingUnsafe" cGridRingUnsafe :: H3Index -> Int -> Ptr H3Index -> IO H3Error

hsGridRingUnsafe :: H3Index -> Int -> (H3Error, [H3Index])
hsGridRingUnsafe origin k = unsafePerformIO $ do
  -- remove 0s from list of H3Index values
  hsGridDiskUsingMethod cGridRingUnsafe origin k >>= (\(h3error, h3indexs) -> return (h3error, filter (/=0) h3indexs))

foreign import capi "h3/h3api.h gridPathCellsSize" cGridPathCellsSize :: H3Index -> H3Index -> Ptr Int64 -> IO H3Error

foreign import capi "h3/h3api.h gridPathCells" cGridPathCells :: H3Index -> H3Index -> Ptr H3Index -> IO H3Error

hsGridPathCells :: H3Index -> H3Index -> (H3Error, [H3Index])
hsGridPathCells origin h3 = 
  unsafePerformIO $ alloca $ \sizePtr -> do
    sizeh3error <- cGridPathCellsSize origin h3 sizePtr
    if sizeh3error == 0
    then do
      size <- fromIntegral <$> peek sizePtr
      allocaArray size $ \resultPtr -> do
        h3error <- cGridPathCells origin h3 resultPtr
        result <- peekArray size resultPtr
        return (h3error, result)
    else do
      return (sizeh3error, [])

-- Note: We are skipping the binding of gridDisksUnsafe at this time, 
--       as we are not certain about the sizing of the output array of H3Index values 
--       at this time.


-- Hierarchy


foreign import capi "h3/h3api.h cellToChildrenSize" cCellToChildrenSize :: H3Index -> Int -> Ptr Int64 -> IO H3Error

foreign import capi "h3/h3api.h cellToChildren" cCellToChildren :: H3Index -> Int -> Ptr H3Index -> IO H3Error

hsCellToChildren :: H3Index -> Int -> (H3Error, [H3Index])
hsCellToChildren cell childRes = unsafePerformIO $ do
  alloca $ \sizePtr -> do
    sizeh3error <- cCellToChildrenSize cell childRes sizePtr
    if sizeh3error == 0
    then do
      size <- fromIntegral <$> peek sizePtr
      allocaArray size $ \resultPtr -> do
        h3error <- cCellToChildren cell childRes resultPtr
        if h3error == 0
        then do
          result <- peekArray size resultPtr
          return (h3error, result)
        else return (h3error, [])
    else return (sizeh3error, [])

-- Normally a method like compactCells would be in H3Api.chs, but the order of the arguments 
-- reduces the benefit of c2hs fun hooks
foreign import capi "h3/h3api.h compactCells" cCompactCells :: Ptr H3Index -> Ptr H3Index -> Int64 -> IO H3Error

hsCompactCells :: [H3Index] -> (H3Error, [H3Index])
hsCompactCells cellSet = unsafePerformIO $ do
  withArray cellSet $ \cellSetPtr -> do
    let size = length cellSet
    allocaArray size $ \compactedSetPtr -> do
      h3error <- cCompactCells cellSetPtr compactedSetPtr (fromIntegral size)
      if h3error == 0
      then do
        result <- peekArray size compactedSetPtr
        return (h3error, result)
      else return (h3error, [])

foreign import capi "h3/h3api.h uncompactCellsSize" cUncompactCellsSize :: Ptr H3Index -> Int64 -> Int -> Ptr Int64 -> IO H3Error

hsUncompactCellsSize :: [H3Index] -> Int -> IO (H3Error, Int64)
hsUncompactCellsSize compactedSet res = do
  withArrayLen compactedSet $ \numCells compactedSetPtr -> do
    alloca $ \maxCellsPtr -> do
      h3error <- cUncompactCellsSize compactedSetPtr (fromIntegral numCells) res maxCellsPtr
      if h3error == 0
      then do
        size <- peek maxCellsPtr
        return (h3error, size)
      else return (h3error, 0)

foreign import capi "h3/h3api.h uncompactCells" cUncompactCells :: Ptr H3Index -> Int64 -> Ptr H3Index -> Int64 -> Int -> IO H3Error

-- TODO: Do we need the following?
hsUncompactCells :: [H3Index] -> Int64 -> Int -> (H3Error, [H3Index])
hsUncompactCells compactedSet maxCells res = unsafePerformIO $ do
  let maxCellsInt = fromIntegral maxCells
  withArrayLen compactedSet $ \numCells compactedSetPtr -> do
    allocaArray maxCellsInt $ \cellSetPtr -> do
      h3error <- cUncompactCells compactedSetPtr (fromIntegral numCells) cellSetPtr maxCells res
      if h3error == 0
      then do
        cellSet <- peekArray maxCellsInt cellSetPtr
        return (h3error, cellSet)
      else return (h3error, [])

hsUncompactCellsUsingSize :: [H3Index] -> Int -> (H3Error, [H3Index])
hsUncompactCellsUsingSize compactedSet res = unsafePerformIO $ do
  -- TODO: Can we just call hsUncompactCellsSize compactedSet res?
  withArrayLen compactedSet $ \numCells compactedSetPtr -> do
    (sizeh3error, maxCells) <- alloca $ \maxCellsPtr -> do
      sizeh3error <- cUncompactCellsSize compactedSetPtr (fromIntegral numCells) res maxCellsPtr
      if sizeh3error == 0
      then do
        size <- peek maxCellsPtr
        return (sizeh3error, size)
      else return (sizeh3error, 0)
    if sizeh3error == 0
    then do
      let maxCellsInt = fromIntegral maxCells
      allocaArray maxCellsInt $ \cellSetPtr -> do
        h3error <- cUncompactCells compactedSetPtr (fromIntegral numCells) cellSetPtr maxCells res
        if h3error == 0
        then do
          cellSet <- peekArray maxCellsInt cellSetPtr
          return (h3error, cellSet)
        else return (h3error, [])
    else return (sizeh3error, [])
{-
numCells, 
H3Index *cellSet, 
const int64_t maxCells,
const int res);
H3Error uncompactCellsSize(const H3Index *compactedSet, const int64_t numCompacted, const int res, int64_t *out);
H3Error uncompactCells(const H3Index *compactedSet, const int64_t numCells, H3Index *cellSet, const int64_t maxCells, const int res);
-}

-- Directed edges


foreign import capi "h3/h3api.h isValidDirectedEdge" cIsValidDirectedEdge :: H3Index -> Int

isValidDirectedEdge :: H3Index -> Bool
isValidDirectedEdge = (/=0) . cIsValidDirectedEdge

foreign import capi "h3/h3api.h directedEdgeToCells" cDirectedEdgeToCells :: H3Index -> Ptr H3Index -> IO H3Error

hsDirectedEdgeToCells :: H3Index -> (H3Error, [H3Index])
hsDirectedEdgeToCells h3index = unsafePerformIO $ do
  allocaArray 2 $ \originDestinationPtr -> do
    h3error <- cDirectedEdgeToCells h3index originDestinationPtr 
    if h3error == 0
    then do
      originDestination <- peekArray 2 originDestinationPtr
      return (h3error, originDestination)
    else return (h3error, [])

foreign import capi "h3/h3api.h originToDirectedEdges" cOriginToDirectedEdges :: H3Index -> Ptr H3Index -> IO H3Error

hsOriginToDirectedEdges :: H3Index -> (H3Error, [H3Index])
hsOriginToDirectedEdges h3index = unsafePerformIO $ do
  allocaArray edgeCount $ \edgesPtr -> do
    h3error <- cOriginToDirectedEdges h3index edgesPtr
    if h3error == 0
    then do
      edges <- peekArray edgeCount edgesPtr
      return (h3error, edges)
    else return (h3error, [])
  where edgeCount = 6


-- Vertexes 


foreign import capi "h3/h3api.h cellToVertexes" cCellToVertexes :: H3Index -> Ptr H3Index -> IO H3Error

hsCellToVertexes :: H3Index -> IO (H3Error, [H3Index])
hsCellToVertexes origin = do
  allocaArray vertexCount $ \outPtr -> do
    h3error <- cCellToVertexes origin outPtr
    if h3error == 0
    then do
      out <- peekArray vertexCount outPtr
      return (h3error, out)
    else return (h3error, [])
  where vertexCount = 6

foreign import capi "h3/h3api.h isValidVertex" cIsValidVertex :: H3Index -> Int

isValidVertex :: H3Index -> Bool
isValidVertex = (/=0) . cIsValidVertex



