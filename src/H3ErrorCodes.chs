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
    poke p (CGeoLoop numVerts verts) = do
        {# set GeoLoop->numVerts #} p numVerts
        {# set GeoLoop->verts #} p verts

newCGeoLoop :: GeoLoop -> IO CGeoLoop
newCGeoLoop gl =
  if length gl > 0
  then do
    ptr <- newArray gl
    return $ CGeoLoop numVerts ptr
  else return $ CGeoLoop 0 nullPtr
  where numVerts = fromIntegral $ length gl
  -- newArray gl >>= (return . CGeoLoop (length gl))

newCGeoLoopPtr :: GeoLoop -> IO (Ptr CGeoLoop)
newCGeoLoopPtr gl = do
  cgl <- newCGeoLoop gl
  ptr <- malloc 
  poke ptr cgl
  return ptr

destroyCGeoLoop :: CGeoLoop -> IO ()
destroyCGeoLoop (CGeoLoop numVerts vertsPtr) = 
  if numVerts > 0
  then do
    free vertsPtr
  else return ()

destroyCGeoLoopPtr :: Ptr CGeoLoop -> IO ()
destroyCGeoLoopPtr ptr = do
  peek ptr >>= destroyCGeoLoop
  free ptr

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
        where peekExterior p = liftM2 CGeoLoop ({# get GeoPolygon->geoloop.numVerts #} p) ({# get GeoPolygon->geoloop.verts #} p)
    poke p (CGeoPolygon (CGeoLoop numVerts verts) numHoles holes) = do
        {# set GeoPolygon->geoloop.numVerts #} p numVerts
        {# set GeoPolygon->geoloop.verts #} p verts
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
    where processPtr ptr = do 
              CLinkedLatLng vertex nextptr <- peek ptr
              followingValues <- extractLatLng nextptr
              return $ vertex : followingValues

data CLinkedGeoLoop = CLinkedGeoLoop 
    { clinkedgeoloop_first :: Ptr CLinkedLatLng
    , clinkedgeoloop_last :: Ptr CLinkedLatLng
    , clinkedgeoloop_next :: Ptr CLinkedGeoLoop
    }
  deriving (Show)


extractGeoLoop :: Ptr CLinkedGeoLoop -> IO [GeoLoop]
extractGeoLoop ptr | ptr /= nullPtr = processPtr ptr
                   | otherwise      = return []
    where processPtr ptr = do 
              CLinkedGeoLoop llfirst _ glnext <- peek ptr
              currentValue <- extractLatLng llfirst
              followingValues <- extractGeoLoop glnext
              return $ currentValue : followingValues

data CLinkedGeoPolygon = CLinkedGeoPolygon 
    { clinkedgeopoly_first :: Ptr CLinkedGeoLoop
    , clinkedgeopoly_last :: Ptr CLinkedGeoLoop
    , clinkedgeopoly_next :: Ptr CLinkedGeoPolygon
    }
  deriving (Show)

extractGeoPolygons :: Ptr CLinkedGeoPolygon -> IO [GeoPolygon]
extractGeoPolygons ptr | ptr /= nullPtr = processPtr ptr
                       | otherwise      = return []
    where processPtr ptr = do 
              CLinkedGeoPolygon glfirst _ gpnext <- peek ptr
              currentGeoLoops <- extractGeoLoop glfirst
              let currentValue = case currentGeoLoops of 
                    exterior : holes -> GeoPolygon exterior holes
                    _                -> GeoPolygon [] []
              followingValues <- extractGeoPolygons gpnext
              return $ currentValue : followingValues

withArrayInput :: (Storable a) => [a] -> ((Ptr a, CInt) -> IO b) -> IO b
withArrayInput as fn =
    withArrayLen as (flip $ curry fnadj)
    where convertInt (ptr, i) = (ptr, fromIntegral i)
          fnadj = fn . convertInt

{-
{# pointer *LinkedGeoPolygon as LinkedGeoPolygonFPtr foreign finalizer destroyLinkedMultiPolygon -> CLinkedGeoPolygon #}
-}
{# pointer *LinkedGeoPolygon as LinkedGeoPolygonFPtr foreign -> CLinkedGeoPolygon #}

{- Might need to make sure we specify alloca using LinkedGeoPolygon
 - We also need a peekLinkedGeoPolygon :: Ptr LinkedGeoPolygon -> GeoPolygon 
        alloca- `GeoPolygon' peekCStringWithLen2*
-}
-- TODO: The following still needs work
{-
{#fun pure cellsToLinkedMultiPolygon as c2hs_cellsToLinkedMultiPolygon
      { withArrayInput* `[H3Index]'&,
        withForeignPtr* `LinkedGeoPolygonFPtr' -- TODO: This is actually an output
      } -> `H3Error' fromIntegral #}
-}

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
    poke p (CLinkedLatLng {-vert-} (LatLng lat lng) next) = do
        {# set LinkedLatLng->vertex.lat #} p {-vert-} (CDouble lat)
        {# set LinkedLatLng->vertex.lng #} p {-vert-} (CDouble lng)
        {# set LinkedLatLng->next #} p (castPtr next)
    {-
    poke p (CLinkedLatLng vert next) = do
        {# set LinkedLatLng->vertex #} p vert
        {# set LinkedLatLng->next #} p next
    -}

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

{-
{#fun pure cellsToLinkedMultiPolygon as c2hs_cellsToLinkedMultiPolygon
      { withArrayInput* `[H3Index]'&,
        alloca- `LinkedGeoPolygonFPtr' newForeignPtr_*
      } -> `H3Error' fromIntegral #}
-}

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

foreign import ccall "h3/h3api.h &destroyLinkedMultiPolygon"
  destroyLinkedMultiPolygon :: FinalizerPtr CLinkedGeoPolygon

{#fun pure cellsToLinkedMultiPolygon as c2hs_cellsToLinkedMultiPolygon2
      { withArrayInput* `[H3Index]'&,
        `LinkedGeoPolygonFPtr'
      } -> `H3Error' fromIntegral #}

hs_cellsToLinkedMultiPolygon :: [H3Index] -> IO (H3Error, [GeoPolygon])
hs_cellsToLinkedMultiPolygon h3indexs = do
  fptr <- mallocForeignPtr
  addForeignPtrFinalizer destroyLinkedMultiPolygon fptr 
  let h3error = c2hs_cellsToLinkedMultiPolygon2 h3indexs fptr
  -- putStrLn $ "hs_cellsToLinkedMultiPolygon status: " ++ show h3error
  if h3error == 0
  then do
    polys <- {-h3error `seq`-} withForeignPtr fptr extractGeoPolygons
    return (h3error, polys)
  else return (h3error, [])

