{-# LANGUAGE ForeignFunctionInterface #-}
module H3.Internal.H3Api
  ( H3ErrorCodes(..)
  , H3Error
  , H3Index
  ) where

import Foreign.C.Types (CULong, CUInt)

#include "h3/h3api.h"

-- |The C H3 error codes are modeled as a Haskell data type and instance of Enum
{# enum H3ErrorCodes {} deriving (Show, Eq) #}

-- |H3Error is the output for most of the funtions in the H3 API. 
--  The C type is uint32_t, in Haskell we use CUInt.
type H3Error = CUInt

-- |H3Index is the numeric representation of the H3 geohashing.  
--  The C type is uint64_t, in Haskell we represent this as CULong.
type H3Index =  CULong

