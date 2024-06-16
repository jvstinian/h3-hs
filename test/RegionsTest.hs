
module RegionsTest
    ( tests
    ) where

import Data.Either (isRight)
import H3.Miscellaneous (degsToRads)
import H3.Data (LatLng(LatLng), GeoPolygon(GeoPolygon))
import H3.Indexing (latLngToCell)
import H3.Regions
  ( polygonToCells
  , cellsToLinkedMultiPolygon
  )
import TestTypes (GenLatLng(GenLatLng))
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Arbitrary (..), choose)


newtype GenRadian = GenRadian Double
  deriving (Eq, Show)

instance Arbitrary GenRadian where
    arbitrary = GenRadian <$> choose (-pi, pi)

tests :: [Test]
tests =
    [ testGroup "Basic functionality check"
        [ testPolygonToCells
        , testCellsToLinkedMultiPolygon 
        ]
    ]

testPolygonToCells :: Test
testPolygonToCells = testProperty "Testing polygonToCells" $ \(GenRadian lat) (GenRadian lng) ->
    let llat = max (lat - 0.01) (-pi)
        ulat = min (lat + 0.01) pi
        llng = max (lng - 0.01) (-pi)
        ulng = min (lng + 0.01) pi
        coords_ll = LatLng (degsToRads llat) (degsToRads llng)
        coords_lr = LatLng (degsToRads llat) (degsToRads ulng)
        coords_ur = LatLng (degsToRads ulat) (degsToRads ulng)
        coords_ul = LatLng (degsToRads ulat) (degsToRads llng)
        gp = GeoPolygon [coords_ll, coords_lr, coords_ur, coords_ul] []
        resultE = polygonToCells gp 5 0
        hasAtLeastOneCell = either (const False) (\h3indexs -> length h3indexs > 0) resultE
    in isRight resultE && hasAtLeastOneCell

testCellsToLinkedMultiPolygon :: Test
testCellsToLinkedMultiPolygon = testProperty "Testing cellsToLinkedMultiPolygon" $ \(GenLatLng coords) ->
    let resultE = latLngToCell coords 9 >>= (\h3index -> cellsToLinkedMultiPolygon [h3index])
        hasAtLeastOneGeoPolygon = either (const False) (not . null) resultE
    in isRight resultE && hasAtLeastOneGeoPolygon

