module H3.Inspection
  ( getResolution
  , getBaseCellNumber
  , stringToH3
  , h3ToString
  , isValidCell
  , isResClassIII
  , isPentagon
  , getIcosahedronFaces
  ) where

import H3.Internal.H3Api 
  ( H3ErrorCodes
  , H3Index
  , c2hs_h3ToString
  , c2hs_stringToH3 )
import H3.Internal.FFI 
  ( getResolution
  , getBaseCellNumber
  , isValidCell
  , isResClassIII
  , isPentagon
  , hsGetIcosahedronFaces )
import H3.Internal.Utils (toEither)

-- |Converts the string representation to the H3Index (Word64) representation.
stringToH3 :: String -> Either H3ErrorCodes H3Index
stringToH3 = toEither . c2hs_stringToH3

-- |Converts the H3Index representation to the string representation.
h3ToString :: H3Index -> Either H3ErrorCodes String
h3ToString = toEither . c2hs_h3ToString

-- | Return all icosahedron faces intersected by a given H3 index.  Faces are represented as integers from 0-19, inclusive. 
--   The array is sparse, and empty (no intersection) array values are represented by -1.
getIcosahedronFaces :: H3Index -> Either H3ErrorCodes [Int]
getIcosahedronFaces = toEither . hsGetIcosahedronFaces

