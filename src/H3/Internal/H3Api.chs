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
  , GeoPolygon
  , CGeoPolygon
  , newCGeoPolygonPtr 
  , destroyCGeoPolygonPtr
  , hsCellsToLinkedMultiPolygon
  ) where

import Control.Monad (liftM2, liftM3)
import Data.Int(Int64)
import Data.Word (Word64, Word32)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.Types (CULong, CLong, CInt(CInt), CDouble(CDouble))
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Marshal.Array (withArrayLen, peekArray, newArray)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Alloc (alloca, malloc, free)
import Foreign.C.String (CString, withCStringLen, peekCString, withCString)
import Foreign.Storable (Storable(peek, poke))
import Foreign.ForeignPtr (withForeignPtr, FinalizerPtr, addForeignPtrFinalizer, mallocForeignPtr)


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

-- Regions

data CGeoLoop = CGeoLoop
    { cgeoloop_numVerts :: CInt
    , cgeoloop_verts :: Ptr LatLng
    }
  deriving (Show)

instance Storable CGeoLoop where
    sizeOf _ = {# sizeof GeoLoop #}
    alignment _ = {# alignof GeoLoop #}
    peek p = liftM2 CGeoLoop ({# get GeoLoop->numVerts #} p)
                             ({# get GeoLoop->verts #} p)
    poke p (CGeoLoop numVerts0 verts0) = do
        {# set GeoLoop->numVerts #} p numVerts0
        {# set GeoLoop->verts #} p verts0

newCGeoLoop :: GeoLoop -> IO CGeoLoop
newCGeoLoop gl =
  if length gl > 0
  then do
    ptr <- newArray gl
    return $ CGeoLoop numVerts0 ptr
  else return $ CGeoLoop 0 nullPtr
  where numVerts0 = fromIntegral $ length gl
  -- newArray gl >>= (return . CGeoLoop (length gl))

{- NOTE: Apparently this is not needed currently
newCGeoLoopPtr :: GeoLoop -> IO (Ptr CGeoLoop)
newCGeoLoopPtr gl = do
  cgl <- newCGeoLoop gl
  ptr <- malloc 
  poke ptr cgl
  return ptr
-}

destroyCGeoLoop :: CGeoLoop -> IO ()
destroyCGeoLoop (CGeoLoop numVerts0 vertsPtr) = 
  if numVerts0 > 0
  then do
    free vertsPtr
  else return ()

{- NOTE: Apparently this is not needed currently
destroyCGeoLoopPtr :: Ptr CGeoLoop -> IO ()
destroyCGeoLoopPtr ptr = do
  peek ptr >>= destroyCGeoLoop
  free ptr
-}

data CGeoPolygon = CGeoPolygon 
    { cgeopoly_exterior :: CGeoLoop
    , cgeopoly_numHoles :: CInt
    , cgeopoly_holes :: Ptr CGeoLoop
    } 
  deriving (Show)

instance Storable CGeoPolygon where
    sizeOf _ = {# sizeof GeoPolygon #}
    alignment _ = {# alignof GeoPolygon #}
    peek p = liftM3 CGeoPolygon {- ({# get GeoPolygon->geoloop #} p) -} (peekExterior p)
                                ({# get GeoPolygon->numHoles #} p)
                                (castPtr <$> {# get GeoPolygon->holes #} p)
        where peekExterior p0 = liftM2 CGeoLoop ({# get GeoPolygon->geoloop.numVerts #} p0) ({# get GeoPolygon->geoloop.verts #} p0)
    poke p (CGeoPolygon (CGeoLoop _numVerts _verts) numHoles holes) = do
        {# set GeoPolygon->geoloop.numVerts #} p _numVerts
        {# set GeoPolygon->geoloop.verts #} p _verts
        {# set GeoPolygon->numHoles #} p numHoles
        {# set GeoPolygon->holes #} p (castPtr holes)

type GeoLoop = [LatLng]

data GeoPolygon = GeoPolygon
   { geopoly_exterior :: GeoLoop
   , geopoly_holes :: [GeoLoop]
   }
  deriving (Eq, Show)

newCGeoPolygon :: GeoPolygon -> IO CGeoPolygon
newCGeoPolygon (GeoPolygon exterior holes) = do
  cext <- newCGeoLoop exterior
  cholesPtr <- if numHoles > 0
               then do
                 choles <- mapM newCGeoLoop holes
                 newArray choles
               else return nullPtr
  return $ CGeoPolygon cext numHoles cholesPtr
  where numHoles = fromIntegral $ length holes

newCGeoPolygonPtr :: GeoPolygon -> IO (Ptr CGeoPolygon)
newCGeoPolygonPtr gp = do
  ptr <- malloc 
  cgp <- newCGeoPolygon gp
  poke ptr cgp
  return ptr

destroyCGeoPolygon :: CGeoPolygon -> IO ()
destroyCGeoPolygon (CGeoPolygon ext numHoles holesPtr) = do
  -- geoloops <- peekArray (fromIntegral numHoles) holesPtr
  -- mapM_ destroyCGeoLoop geoloops
  if numHoles > 0
  then do
    peekArray (fromIntegral numHoles) holesPtr >>= mapM_ destroyCGeoLoop 
    free holesPtr
  else return ()
  destroyCGeoLoop ext

destroyCGeoPolygonPtr :: Ptr CGeoPolygon -> IO ()
destroyCGeoPolygonPtr ptr = do
  peek ptr >>= destroyCGeoPolygon
  free ptr

data CLinkedLatLng = CLinkedLatLng
    { clinkedlatlng_vertex :: LatLng
    --  clinkedlatlng_lat :: Double
    --, clinkedlatlng_lng :: Double
    , clinkedlatlng_next :: Ptr CLinkedLatLng -- () # NOTE: Had this before, not sure why
    }
  deriving (Show)

extractLatLng :: Ptr CLinkedLatLng -> IO [LatLng]
extractLatLng ptr | ptr /= nullPtr = processPtr ptr
                  | otherwise      = return []
    where processPtr ptr0 = do 
              CLinkedLatLng vertex nextptr <- peek ptr0
              followingValues <- extractLatLng nextptr
              return $ vertex : followingValues

-- Trying the following instead
instance Storable CLinkedLatLng where
    sizeOf _ = {# sizeof LinkedLatLng #}
    alignment _ = {# alignof LinkedLatLng #}
    peek p = do
        CDouble llat <- {# get LinkedLatLng->vertex.lat #} p
        CDouble llng <- {# get LinkedLatLng->vertex.lng #} p
        llptr <- {# get LinkedLatLng->next #} p
        return $ CLinkedLatLng (LatLng llat llng) (castPtr llptr)
    {-
    peek p = CLinkedLatLng <$> {# get LinkedLatLng->vertex #} p
                           <*> {# get LinkedLatLng->next #} p
    -}
    poke p (CLinkedLatLng {-vert-} (LatLng latval lngval) next) = do
        {# set LinkedLatLng->vertex.lat #} p {-vert-} (CDouble latval)
        {# set LinkedLatLng->vertex.lng #} p {-vert-} (CDouble lngval)
        {# set LinkedLatLng->next #} p (castPtr next)
    {-
    poke p (CLinkedLatLng vert next) = do
        {# set LinkedLatLng->vertex #} p vert
        {# set LinkedLatLng->next #} p next
    -}

data CLinkedGeoLoop = CLinkedGeoLoop 
    { clinkedgeoloop_first :: Ptr CLinkedLatLng
    , clinkedgeoloop_last :: Ptr CLinkedLatLng
    , clinkedgeoloop_next :: Ptr CLinkedGeoLoop
    }
  deriving (Show)

extractGeoLoop :: Ptr CLinkedGeoLoop -> IO [GeoLoop]
extractGeoLoop ptr | ptr /= nullPtr = processPtr ptr
                   | otherwise      = return []
    where processPtr ptr0 = do 
              CLinkedGeoLoop llfirst _ glnext <- peek ptr0
              currentValue <- extractLatLng llfirst
              followingValues <- extractGeoLoop glnext
              return $ currentValue : followingValues

instance Storable CLinkedGeoLoop where
    sizeOf _ = {# sizeof LinkedGeoLoop #}
    alignment _ = {# alignof LinkedGeoLoop #}
    peek p = liftM3 CLinkedGeoLoop (castPtr <$> {# get LinkedGeoLoop->first #} p) 
                                   (castPtr <$> {# get LinkedGeoLoop->last #} p) 
                                   (castPtr <$> {# get LinkedGeoLoop->next #} p)
    poke p (CLinkedGeoLoop cllfirst clllast cllnext) = do
        {# set LinkedGeoLoop->first #} p (castPtr cllfirst)
        {# set LinkedGeoLoop->last #} p (castPtr clllast)
        {# set LinkedGeoLoop->next #} p (castPtr cllnext)

data CLinkedGeoPolygon = CLinkedGeoPolygon 
    { clinkedgeopoly_first :: Ptr CLinkedGeoLoop
    , clinkedgeopoly_last :: Ptr CLinkedGeoLoop
    , clinkedgeopoly_next :: Ptr CLinkedGeoPolygon
    }
  deriving (Show)

extractGeoPolygons :: Ptr CLinkedGeoPolygon -> IO [GeoPolygon]
extractGeoPolygons ptr | ptr /= nullPtr = processPtr ptr
                       | otherwise      = return []
    where processPtr ptr0 = do 
              CLinkedGeoPolygon glfirst _ gpnext <- peek ptr0
              currentGeoLoops <- extractGeoLoop glfirst
              let currentValue = case currentGeoLoops of 
                    exterior : holes -> GeoPolygon exterior holes
                    _                -> GeoPolygon [] []
              followingValues <- extractGeoPolygons gpnext
              return $ currentValue : followingValues

instance Storable CLinkedGeoPolygon where
    sizeOf _ = {# sizeof LinkedGeoPolygon #}
    alignment _ = {# alignof LinkedGeoPolygon #}
    -- peek p = liftM3 CLinkedGeoPolygon ({# get LinkedGeoPolygon->first #} p) ({# get LinkedGeoPolygon->last #} p) ({# get LinkedGeoPolygon->next #} p)
    peek p = liftM3 CLinkedGeoPolygon (castPtr <$> ({# get LinkedGeoPolygon->first #} p)) 
                                      (castPtr <$> ({# get LinkedGeoPolygon->last #} p))
                                      (castPtr <$> ({# get LinkedGeoPolygon->next #} p))
    poke p (CLinkedGeoPolygon clgfirst clglast clgnext) = do
        {# set LinkedGeoPolygon->first #} p (castPtr clgfirst)
        {# set LinkedGeoPolygon->last #} p (castPtr clglast)
        {# set LinkedGeoPolygon->next #} p (castPtr clgnext)
    -- poke p (CLinkedGeoPolygon clgfirst clglast clgnext) = do
    --     {# set LinkedGeoPolygon->first #} p clgfirst
    --     {# set LinkedGeoPolygon->last #} p clglast
    --     {# set LinkedGeoPolygon->next #} p clgnext

withArrayInput :: (Storable a) => [a] -> ((Ptr a, CInt) -> IO b) -> IO b
withArrayInput as fn =
    withArrayLen as (flip $ curry fnadj)
    where convertInt (ptr, i) = (ptr, fromIntegral i)
          fnadj = fn . convertInt

{-
{# pointer *LinkedGeoPolygon as LinkedGeoPolygonFPtr foreign finalizer destroyLinkedMultiPolygon -> CLinkedGeoPolygon #}
-}

{- NOTE: The following did not work
{#fun pure cellsToLinkedMultiPolygon as c2hs_cellsToLinkedMultiPolygon
      { withArrayInput* `[H3Index]'&,
        withForeignPtr* `LinkedGeoPolygonFPtr' -- TODO: This is actually an output
      } -> `H3Error' fromIntegral #}
-}

{- I don't think this worked either.  I don't think this set up the finalizer.
{#fun pure cellsToLinkedMultiPolygon as c2hs_cellsToLinkedMultiPolygon
      { withArrayInput* `[H3Index]'&,
        alloca- `LinkedGeoPolygonFPtr' newForeignPtr_*
      } -> `H3Error' fromIntegral #}
-}

{- Original attempt
newPolygonFPtr :: Ptr CLinkedGeoPolygon -> IO LinkedGeoPolygonFPtr
newPolygonFPtr ptr = do
    fptr <- newForeignPtr destroyLinkedMultiPolygon ptr
    {- free(): invalid size
    fptr <- newForeignPtr finalizerFree ptr -- or mallocForeignPtr
    addForeignPtrFinalizer destroyLinkedMultiPolygon fptr 
    -}
    {- This seems to work
    fptr <- newForeignPtr_ ptr
    addForeignPtrFinalizer destroyLinkedMultiPolygon fptr 
    -}
    return fptr

{#fun pure cellsToLinkedMultiPolygon as c2hs_cellsToLinkedMultiPolygon
      { withArrayInput* `[H3Index]'&,
        alloca- `LinkedGeoPolygonFPtr' newPolygonFPtr*
      } -> `H3Error' fromIntegral #}
-}

withH3IndexArray :: [H3Index] -> ((Ptr CULong, CInt) -> IO b) -> IO b
withH3IndexArray = withArrayInput . (map fromIntegral)

foreign import ccall "h3/h3api.h &destroyLinkedMultiPolygon"
  destroyLinkedMultiPolygon :: FinalizerPtr CLinkedGeoPolygon

{# pointer *LinkedGeoPolygon as LinkedGeoPolygonFPtr foreign -> CLinkedGeoPolygon #}

{#fun pure cellsToLinkedMultiPolygon as c2hs_cellsToLinkedMultiPolygon2
      { withH3IndexArray* `[H3Index]'&,
        `LinkedGeoPolygonFPtr'
      } -> `H3Error' fromIntegral #}

hsCellsToLinkedMultiPolygon :: [H3Index] -> (H3Error, [GeoPolygon])
hsCellsToLinkedMultiPolygon h3indexs = unsafePerformIO $ do
  fptr <- mallocForeignPtr
  addForeignPtrFinalizer destroyLinkedMultiPolygon fptr 
  let h3error = c2hs_cellsToLinkedMultiPolygon2 h3indexs fptr
  if h3error == 0
  then do
    polys <- withForeignPtr fptr extractGeoPolygons
    return (h3error, polys)
  else return (h3error, [])

{-
hsCellsToLinkedMultiPolygon :: [H3Index] -> (H3Error, [GeoPolygon])
hsCellsToLinkedMultiPolygon h3indexs = $ hsCellsToLinkedMultiPolygonIO h3indexs
-}

-- Miscellaneous 


peekDouble :: Ptr CDouble -> IO Double
peekDouble ptr = cdoubleToDouble <$> peek ptr
  where cdoubleToDouble (CDouble x) = x

-- TODO: The following is similar to peekAsH3Index, so possibly need to DRY this up
peekInt64 :: Ptr CLong -> IO Int64
peekInt64 ptr = fromIntegral <$> peek ptr

{#fun pure getHexagonAreaAvgKm2 as c2hs_getHexagonAreaAvgKm2 
      { `Int',
        alloca- `Double' peekDouble*
      } -> `H3Error' fromIntegral #}

{#fun pure getHexagonAreaAvgM2 as c2hs_getHexagonAreaAvgM2 
      { `Int',
        alloca- `Double' peekDouble*
      } -> `H3Error' fromIntegral #}

{#fun pure cellAreaRads2 as c2hs_cellAreaRads2
      { fromIntegral `H3Index',
        alloca- `Double' peekDouble*
      } -> `H3Error' fromIntegral #}

{#fun pure cellAreaKm2 as c2hs_cellAreaKm2
      { fromIntegral `H3Index',
        alloca- `Double' peekDouble*
      } -> `H3Error' fromIntegral #}

{#fun pure cellAreaM2 as c2hs_cellAreaM2
      { fromIntegral `H3Index',
        alloca- `Double' peekDouble*
      } -> `H3Error' fromIntegral #}

{#fun pure getHexagonEdgeLengthAvgKm as c2hs_getHexagonEdgeLengthAvgKm
      { `Int',
        alloca- `Double' peekDouble*
      } -> `H3Error' fromIntegral #}

{#fun pure getHexagonEdgeLengthAvgM as c2hs_getHexagonEdgeLengthAvgM
      { `Int',
        alloca- `Double' peekDouble*
      } -> `H3Error' fromIntegral #}

{#fun pure edgeLengthRads as c2hs_edgeLengthRads
      { fromIntegral `H3Index',
        alloca- `Double' peekDouble*
      } -> `H3Error' fromIntegral #}

{#fun pure edgeLengthKm as c2hs_edgeLengthKm
      { fromIntegral `H3Index',
        alloca- `Double' peekDouble*
      } -> `H3Error' fromIntegral #}

{#fun pure edgeLengthM as c2hs_edgeLengthM
      { fromIntegral `H3Index',
        alloca- `Double' peekDouble*
      } -> `H3Error' fromIntegral #}

{#fun pure getNumCells as c2hs_getNumCells
      { `Int',
        alloca- `Int64' peekInt64*
      } -> `H3Error' fromIntegral #}

{#fun pure greatCircleDistanceKm as c2hs_greatCircleDistanceKm
      { with* `LatLng',
        with* `LatLng'
      } -> `Double' #}

{#fun pure greatCircleDistanceM as c2hs_greatCircleDistanceM
      { with* `LatLng',
        with* `LatLng'
      } -> `Double' #}

{#fun pure greatCircleDistanceRads as c2hs_greatCircleDistanceRads
      { with* `LatLng',
        with* `LatLng'
      } -> `Double' #}

