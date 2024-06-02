{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module H3.Internal.FFI
  ( degsToRads
  , radsToDegs
  ) where

foreign import capi "h3/h3api.h degsToRads" degsToRads :: Double -> Double

foreign import capi "h3/h3api.h radsToDegs" radsToDegs :: Double -> Double

