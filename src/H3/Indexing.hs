module H3.Indexing
  ( LatLng(LatLng)
  , latLngToCell
  , cellToLatLng
  , cellToBoundary
  ) where

-- import Data.Word (Word64)
import H3.Internal.H3Api 
  ( LatLng(LatLng)
  , H3Error
  , H3ErrorCodes
  , H3Index
  , c2hs_latLngToCell
  , c2hs_cellToLatLng
  , c2hs_cellToBoundary )


-- type H3Index = Word64

toEither :: (H3Error, a) -> Either H3ErrorCodes a
toEither (h3error, res) =
  if h3error /= 0
  then Left ((toEnum . fromIntegral) h3error)
  else Right res

latLngToCell :: LatLng -> Int -> Either H3ErrorCodes H3Index
latLngToCell coords = toEither . c2hs_latLngToCell coords --   (h3error, fromIntegral ch3index)
  -- where (h3error, h3index) = c2hs_latLngToCell coords res
        -- h3index = fromIntegral ch3index

cellToLatLng :: H3Index -> Either H3ErrorCodes LatLng
cellToLatLng = toEither . c2hs_cellToLatLng -- . fromIntegral

cellToBoundary :: H3Index -> Either H3ErrorCodes [LatLng] 
cellToBoundary = toEither . c2hs_cellToBoundary -- . fromIntegral

