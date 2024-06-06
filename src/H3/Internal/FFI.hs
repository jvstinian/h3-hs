{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module H3.Internal.FFI
  ( degsToRads
  , radsToDegs
  , isValidCell
  ) where

import H3.Internal.H3Api (H3Index)


-- |degsToRads converts from degrees to radians.
foreign import capi "h3/h3api.h degsToRads" degsToRads :: Double -> Double

-- |radsToDegs converts from radians to degrees
foreign import capi "h3/h3api.h radsToDegs" radsToDegs :: Double -> Double

-- |isValidCell returns non-zero if this is a valid H3 cell index
foreign import capi "h3/h3api.h isValidCell" isValidCell :: H3Index -> Int

