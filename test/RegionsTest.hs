
module RegionsTest
    ( tests
    ) where

import Data.Either (isRight)
import H3.Miscellaneous (degsToRads)
import H3.Indexing 
  ( LatLng(LatLng)
  , latLngToCell
  )
import H3.Regions
  ( polygonToCells
  , cellsToLinkedMultiPolygon
  , GeoPolygon(GeoPolygon)
  )
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- import Test.QuickCheck                      (Arbitrary (..), chooseInt, choose)

tests :: [Test]
tests =
    [ testGroup "Basic functionality check"
        [ testPolygonToCells
        , testCellsToLinkedMultiPolygon 
        ]
    ]

testPolygonToCells :: Test
testPolygonToCells = testProperty "Testing polygonToCells" $ 
    isRight resultE && hasAtLeastOneCell
    where
        coords_ll = LatLng (degsToRads 45 - 0.01) (degsToRads (-72.5 - 0.01))
        coords_lr = LatLng (degsToRads 45 - 0.01) (degsToRads (-72.5 + 0.01))
        coords_ur = LatLng (degsToRads 45 + 0.01) (degsToRads (-72.5 + 0.01))
        coords_ul = LatLng (degsToRads 45 + 0.01) (degsToRads (-72.5 - 0.01))
        gp = GeoPolygon [coords_ll, coords_lr, coords_ur, coords_ul] []
        resultE = polygonToCells gp 5 0
        hasAtLeastOneCell = either (const False) (\h3indexs -> length h3indexs > 0) resultE

testCellsToLinkedMultiPolygon :: Test
testCellsToLinkedMultiPolygon = testProperty "Testing cellsToLinkedMultiPolygon" $ 
    isRight resultE && hasAtLeastOneGeoPolygon
    where
        coords = LatLng (degsToRads 45) (degsToRads (-72.5))
        resultE = latLngToCell coords 9 >>= (\h3index -> cellsToLinkedMultiPolygon [h3index])
        hasAtLeastOneGeoPolygon = either (const False) (\geopolys -> length geopolys > 0) resultE

