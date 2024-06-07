
module RegionsTest
    ( tests
    ) where

import Data.Either (isRight)
import H3.Miscellaneous (degsToRads)
import H3.Indexing 
  ( LatLng(LatLng)
  )
import H3.Regions
  ( polygonToCells
  , GeoPolygon(GeoPolygon)
  )
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- import Test.QuickCheck                      (Arbitrary (..), chooseInt, choose)

tests :: [Test]
tests =
    [ testGroup "Basic functionality check"
        [ testPolygonToCells
        ]
    ]

testPolygonToCells :: Test
testPolygonToCells = testProperty "Testing polygonToCells" $ 
    isRight resultE
    where
        coords_ll = LatLng (degsToRads 45 - 0.01) (degsToRads (-72.5 - 0.01))
        coords_lr = LatLng (degsToRads 45 - 0.01) (degsToRads (-72.5 + 0.01))
        coords_ur = LatLng (degsToRads 45 + 0.01) (degsToRads (-72.5 + 0.01))
        coords_ul = LatLng (degsToRads 45 + 0.01) (degsToRads (-72.5 - 0.01))
        gp = GeoPolygon [coords_ll, coords_lr, coords_ur, coords_ul] []
        resultE = polygonToCells gp 5 0

