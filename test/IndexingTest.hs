module IndexingTest
    ( tests
    ) where

import Control.Monad (liftM2)
import Data.Either (isRight)
import H3.Data (LatLng(LatLng))
import H3.Indexing (latLngToCell, cellToLatLng, cellToBoundary)
import H3.Miscellaneous (degsToRads, radsToDegs)
import H3.Inspection (h3ToString, stringToH3)

import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)


tests :: [Test]
tests =
    [ testGroup "LatLng To Cell"
        [ latLngToCellWithKnownValues
        , testLatLngToAddressWithCenterValues
        ]
    , testGroup "Cell To LatLng"
        [ cellToLatLngWithKnowValue
        , testAddressToLatLngWithCenterValues
        ]
    , testGroup "Cell To Boundary"
        [ testCellToBoundary ]
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

(=~) :: LatLng -> LatLng -> Bool
(=~) (LatLng lat1 lng1) (LatLng lat2 lng2) =
  abs (lat1 - lat2) < tol && abs (lng1 - lng2) < tol
  where tol = 1e-10

-- The following was taken from https://github.com/uber/h3/blob/master/tests/cli/cellToLatLng.txt
cellToLatLngWithKnowValue :: Test
cellToLatLngWithKnowValue = testProperty "Testing known conversion from cell string to latitude-longitude" $
    isRight latLngE && check
    where
        latLngRadsToDegs (LatLng lat lng) = LatLng (radsToDegs lat) (radsToDegs lng)
        latLngE = (stringToH3 "8928342e20fffff") >>= cellToLatLng >>= (return . latLngRadsToDegs)
        expectedResultE = Right $ LatLng 37.5012466151 (-122.5003039349)
        check = either (const False) id (liftM2 (=~) latLngE expectedResultE)

-- The following was taken from https://github.com/uber/h3/blob/master/tests/cli/cellToBoundary.txt
-- Note that the CLI test lists 7 LatLng pairs in the polygon, where the first is identical to the last.
-- The C API (through this Haskell binding) instead returns 6 LatLng values.
-- The Python package (though on h3 version <4) also returns 6 LatLng values.
testCellToBoundary :: Test
testCellToBoundary = testProperty "Testing known value of cell to boundary" $
    isRight actualResultE && lengthCheck && valCheck
    where 
        h3str = "8928342e20fffff"
        latLngRadsToDegs (LatLng lat lng) = LatLng (radsToDegs lat) (radsToDegs lng)
        actualResultE = stringToH3 h3str >>= cellToBoundary >>= (return . map latLngRadsToDegs)
        expectedBoundary = 
            [ LatLng 37.4997389893 (-122.4990471431)
            , LatLng 37.5014245698 (-122.4979805011)
            , LatLng 37.5029321860 (-122.4992373065) 
            , LatLng 37.5027541980 (-122.5015607527) 
            , LatLng 37.5010686174 (-122.5026273256)
            , LatLng 37.4995610248 (-122.5013705214) 
            ]
        expectedResultE = Right expectedBoundary
        lengthCheck = either (const False) id (liftM2 (\lls1 lls2 -> length lls1 == length lls2) actualResultE expectedResultE)
        valCheck = either (const False) id (liftM2 (\lls1 lls2 -> and (map (uncurry (=~)) (zip lls1 lls2)))  actualResultE expectedResultE)
        
-- The following are taken from the files "*centers.txt" located at 
-- https://github.com/uber/h3/blob/master/tests/inputfiles
latLngTestValues :: [(Int, String, LatLng)]
latLngTestValues =
  [ ( 8, "880a000001fffff", LatLng (64.436597) (89.573069) )
  , ( 8, "880a000003fffff", LatLng (64.442945) (89.584180) )
  , ( 8, "880a000005fffff", LatLng (64.428979) (89.581150) )
  , ( 8, "880a000007fffff", LatLng (64.435326) (89.592260) )
  , ( 8, "880a000009fffff", LatLng (64.437865) (89.553875) )
  , ( 8, "880a00000bfffff", LatLng (64.444214) (89.564982) )
  , ( 8, "880a00000dfffff", LatLng (64.430248) (89.561962) )
  , ( 11, "8b0a00000000fff", LatLng (64.436597) (89.573069) )
  , ( 11, "8b0a00000001fff", LatLng (64.437011) (89.573357) )
  , ( 11, "8b0a00000002fff", LatLng (64.436260) (89.573790) )
  , ( 11, "8b0a00000003fff", LatLng (64.436674) (89.574079) )
  , ( 11, "8b0a00000004fff", LatLng (64.436519) (89.572058) )
  , ( 11, "8b0a00000005fff", LatLng (64.436933) (89.572347) )
  , ( 15, "8f0a00000000000", LatLng (64.436597) (89.573069) )
  , ( 15, "8f0a00000000001", LatLng (64.436605) (89.573074) )
  , ( 15, "8f0a00000000002", LatLng (64.436590) (89.573083) )
  , ( 15, "8f0a00000000003", LatLng (64.436598) (89.573089) )
  , ( 15, "8f0a00000000004", LatLng (64.436595) (89.573048) )
  , ( 15, "8f0a00000000005", LatLng (64.436603) (89.573054) )
  ]

testLatLngToAddressWithCenterValues :: Test
testLatLngToAddressWithCenterValues = testProperty "Testing coordinate to address mapping with known center values" $
    either (const False) id testResult
    where
        testResult = and <$> mapM processRecord latLngTestValues
        -- For a single latitude-longitude pair, convert to radians, get the cell, then convert to cell address, and finally 
        -- check against the provided cell address
        processRecord (res, cellAddr, latLng) = (latLngToCell (convertToRads latLng) res) >>= h3ToString >>= (return . (==cellAddr))
        convertToRads (LatLng lat lng) = LatLng (degsToRads lat) (degsToRads lng)

testAddressToLatLngWithCenterValues :: Test
testAddressToLatLngWithCenterValues = testProperty "Testing address to coordinate mapping with known center values" $
    either (const False) id testResult
    where
        testResult = and <$> mapM processRecord latLngTestValues
        -- For a single latitude-longitude pair, convert address to H3 index, then to coordinates, change from radians to degrees, 
        -- and finally check against the provided latitude and longitude
        processRecord (_, cellAddr, latLng) = (approxEq latLng) <$> latLngRadsToDegs <$> ((stringToH3 cellAddr) >>= cellToLatLng)
        latLngRadsToDegs (LatLng lat lng) = LatLng (radsToDegs lat) (radsToDegs lng)

        approxEq :: LatLng -> LatLng -> Bool
        approxEq (LatLng lat1 lng1) (LatLng lat2 lng2) = abs (lat1 - lat2) < tol && abs (lng1 - lng2) < tol
            where tol = 1e-6

