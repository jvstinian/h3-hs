module IndexingTest
    ( tests
    ) where

import Data.Either (isRight)
import H3.Indexing (LatLng(LatLng), latLngToCell)
import H3.Miscellaneous (degsToRads)
import H3.Inspection (h3ToString)

import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)


tests :: [Test]
tests =
    [ testGroup "LatLng To Cell"
        [ latLngToCellWithKnownValues
        ]
    ]

-- The following implements https://github.com/uber/h3/blob/master/tests/cli/latLngToCell.txt
latLngToCellWithKnownValues :: Test
latLngToCellWithKnownValues = testProperty "Testing known conversion from latitude-longitude to cell string" $
    isRight h3strE && h3strE == expectedResultE
    where
        latRads = degsToRads 20
        lngRads = degsToRads 123
        inputLatLng = (LatLng latRads lngRads)
        inputResolution = 2 
        h3indexE = latLngToCell inputLatLng inputResolution
        h3strE = h3indexE >>= h3ToString
        expectedResultE = Right "824b9ffffffffff"

