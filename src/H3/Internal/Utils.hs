module H3.Internal.Utils
  ( toEither
  ) where

import H3.Internal.H3Api 
  ( H3Error
  , H3ErrorCodes )


toEither :: (H3Error, a) -> Either H3ErrorCodes a
toEither (h3error, res) =
  if h3error /= 0
  then Left ((toEnum . fromIntegral) h3error)
  else Right res

