module H3.Inspection
  ( h3ToString
  , stringToH3
  , isValidCell
  ) where

import H3.Internal.H3Api 
  ( H3ErrorCodes
  , H3Index
  , c2hs_h3ToString
  , c2hs_stringToH3 )
import H3.Internal.FFI (isValidCell)
import H3.Internal.Utils (toEither)

stringToH3 :: String -> Either H3ErrorCodes H3Index
stringToH3 = toEither . c2hs_stringToH3

h3ToString :: H3Index -> Either H3ErrorCodes String
h3ToString = toEither . c2hs_h3ToString

