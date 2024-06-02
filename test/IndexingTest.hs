-- {-# LANGUAGE FlexibleContexts #-}

module IndexingTest
    ( tests
    ) where

import Miscellaneous (degsToRads, hsLatLngToCell, h3ToString)
import H3ErrorCodes (LatLng(LatLng), H3ErrorCodes(E_SUCCESS), H3Error)

import System.IO.Unsafe (unsafePerformIO)
-- import Test.HUnit                           (assertEqual)
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
{-
import Test.QuickCheck                      (Arbitrary, NonNegative (..), Positive (..),
                                             (==>))
-}

tests :: [Test]
tests =
    [ testGroup "LatLng To Cell"
        [ latLngToCellWithKnownValues
        ]
    ]


-- The following implements https://github.com/uber/h3/blob/master/tests/cli/latLngToCell.txt
latLngToCellWithKnownValues :: Test
latLngToCellWithKnownValues = testProperty "Testing known conversion from latitude-longitude to cell string" $
    (toEnum . fromIntegral) err0 == E_SUCCESS && (toEnum . fromIntegral) err1 == E_SUCCESS && h3str == expectedResult
    where
        latRads = degsToRads 20
	lngRads = degsToRads 123
        inputLatLng = (LatLng latRads lngRads)
	inputResolution = 2 
        (h3index, err0) = unsafePerformIO $ hsLatLngToCell inputLatLng inputResolution
	(h3str, err1) = unsafePerformIO $ h3ToString h3index
	expectedResult = "824b9ffffffffff"

