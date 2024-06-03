module IndexingTest
    ( tests
    ) where

import Control.Monad (liftM2)
import Data.Either (isRight)
import H3.Indexing (LatLng(LatLng), H3ErrorCodes(E_CELL_INVALID), latLngToCell, cellToLatLng, cellToBoundary)
import H3.Miscellaneous (degsToRads, radsToDegs)
import H3.Inspection (h3ToString, stringToH3)

import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)


tests :: [Test]
tests =
    [ testGroup "LatLng To Cell"
        [ latLngToCellWithKnownValues
        ]
    , testGroup "Cell To LatLng"
        [ cellToLatLngWithKnowValue
        -- , testInvalidCellToLatLng
        , testCellToBoundary
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
-- add_h3_cli_test(testCliCellToBoundary "cellToBoundary -c 8928342e20fffff" "POLYGON((-122.4990471431 37.4997389893, -122.4979805011 37.5014245698, -122.4992373065 37.5029321860, -122.5015607527 37.5027541980, -122.5026273256 37.5010686174, -122.5013705214 37.4995610248, -122.4990471431 37.4997389893))")
-- Note that the CLI test lists 7 LatLng pairs in the polygon, where the first is identical to the last.
-- The C API (through this Haskell binding) instead returns 6 LatLng values.
-- The Python package (though on <4) also returns 6 LatLng values.
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
            -- , LatLng 37.4997389893 (-122.4990471431) 
            ]
        expectedResultE = Right expectedBoundary
        lengthCheck = either (const False) id (liftM2 (\lls1 lls2 -> length lls1 == length lls2) actualResultE expectedResultE)
        valCheck = either (const False) id (liftM2 (\lls1 lls2 -> and (map (uncurry (=~)) (zip lls1 lls2)))  actualResultE expectedResultE)
        
-- TODO: Might need to drop this test
testInvalidCellToLatLng :: Test
testInvalidCellToLatLng = testProperty "Testing invalid cell value" $
    latLngE == expectedResultE
    where
        latLngRadsToDegs (LatLng lat lng) = LatLng (radsToDegs lat) (radsToDegs lng)
        latLngE = (stringToH3 "asdf") >>= cellToLatLng >>= (return . latLngRadsToDegs)
        expectedResultE = Left E_CELL_INVALID

