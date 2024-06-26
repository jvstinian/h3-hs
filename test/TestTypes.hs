module TestTypes
    ( GenLatLng(GenLatLng, fromGenLatLng)
    , Resolution(Resolution)
    ) where

import Control.Monad (liftM2)
import H3.Data (LatLng(LatLng))
import Test.QuickCheck (Arbitrary (..), chooseInt, choose)

newtype Resolution = Resolution Int
  deriving (Eq, Show)

instance Arbitrary Resolution where
    arbitrary = Resolution <$> chooseInt (0, 15)

newtype GenLatLng = GenLatLng { fromGenLatLng :: LatLng }
  deriving (Eq, Show)

instance Arbitrary GenLatLng where
    arbitrary = GenLatLng <$> liftM2 LatLng (choose (-pi, pi)) (choose (-pi, pi))

