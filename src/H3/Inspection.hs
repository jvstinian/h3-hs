module H3.Inspection
  ( h3ToString
  ) where

import H3.Internal.H3Api 
  ( H3ErrorCodes
  , H3Index
  , c2hs_h3ToString )
import H3.Internal.Utils (toEither)

h3ToString :: H3Index -> Either H3ErrorCodes String
h3ToString = toEither . c2hs_h3ToString

