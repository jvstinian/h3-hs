module MiscellaneousTest
    ( tests
    ) where

import H3.Indexing (latLngToCell)
import H3.Miscellaneous 
  ( degsToRads
  , radsToDegs
  , getRes0Cells
  , getPentagons
  , getHexagonAreaAvgKm2
  , getHexagonAreaAvgM2
  , getHexagonEdgeLengthAvgKm
  , getHexagonEdgeLengthAvgM
  , cellAreaKm2
  , cellAreaM2
  , getNumCells
  , greatCircleDistanceKm
  , greatCircleDistanceM
  , greatCircleDistanceRads
  )
import Control.Monad (liftM2)
import TestTypes     (GenLatLng(GenLatLng), Resolution(Resolution))
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      ()

tests :: [Test]
tests =
    [ testGroup "Check conversion between degrees and radians"
        [ checkDegreesToRadiansConversion
        , checkRadiansToDegreesConversion
        , checkDegreesConversionRoundtrip
        ]
    , testGroup "Check counts"
        [ testGetRes0CellsReturns122Cells
        , testGetPentagonsReturns12Cells 
        , testGetNumCells 
        ]
    , testGroup "Check unit conversions"
        [ testHexagonAreaAvgConversion
        , testCellAreaConversion
        , testHexagonEdgeLengthAvgConversion
        , testGreatCircleDistanceConversion 
        ]
    , testGroup "Check monotonicity"
        [ testMonotonicityWithResolution "getHexagonAreaAvgKm2" getHexagonAreaAvgKm2
        , testMonotonicityWithResolution "getHexagonAreaAvgM2" getHexagonAreaAvgM2
        , testMonotonicityWithResolution "getHexagonEdgeLengthAvgKm" getHexagonEdgeLengthAvgKm
        , testMonotonicityWithResolution "getHexagonEdgeLengthAvgM" getHexagonEdgeLengthAvgM
        ]
    , testGroup "Check distance"
        [ testGreatCircleRadsNonzero 
        ]

    ]

eps :: RealFloat a => a
eps = eps'
    where
        eps' = encodeFloat 1 (1 - floatDigits eps')

checkDegreesToRadiansConversion :: Test
checkDegreesToRadiansConversion = testProperty "degrees to radians conversion" $ \degs ->
    let expectedRads = degs * pi / 180
        actualRads = degsToRads degs
        tol = 16 * eps
    in abs (expectedRads - actualRads) < tol

checkRadiansToDegreesConversion :: Test
checkRadiansToDegreesConversion = testProperty "radians to degrees conversion" $ \rads ->
    let expectedDegs = rads * 180 / pi
        actualDegs = radsToDegs rads
        tol = 8192 * eps
    in abs (expectedDegs - actualDegs) < tol
 
checkDegreesConversionRoundtrip :: Test
checkDegreesConversionRoundtrip = testProperty "degrees to radians conversion" $ \degs ->
    let expected = degs
        actual = (radsToDegs . degsToRads) degs
        tol = 256 * eps
    in abs (expected - actual) < tol

testGetRes0CellsReturns122Cells :: Test
testGetRes0CellsReturns122Cells = testProperty "getRes0Cells returns 122 cells" $ do
    actualResultE == expectedResultE
    where expectedResultE = Right 122
          actualResultE = length <$> getRes0Cells 

testGetPentagonsReturns12Cells :: Test
testGetPentagonsReturns12Cells = testProperty "getPentagons returns 12 cells" $ do
    actualResultE == expectedResultE
    where resolutions = [0..15]
          actualResultE = mapM (fmap length . getPentagons) resolutions
          expectedResultE = Right (replicate (length resolutions) 12)

testHexagonAreaAvgConversion :: Test
testHexagonAreaAvgConversion = testProperty "unit conversion from getHexagonAreaAvgKm2 to getHexagonAreaAvgM2" $ do
    either (const False) and checkResolutionValues
    where resolutions = [0..15]
          convertKm2ToM2 = (*) (1000.0 ** 2)
          actualResultE = mapM (fmap convertKm2ToM2 . getHexagonAreaAvgKm2) resolutions
          expectedResultE = mapM getHexagonAreaAvgM2 resolutions
          tol = 1e-2
          listsApproximatelyEqual as bs = zipWith (\a b -> abs (a - b) < tol) as bs
          checkResolutionValues = liftM2 listsApproximatelyEqual actualResultE expectedResultE 

testMonotonicityWithResolution :: (Ord b) => String -> (Int -> Either err b) -> Test
testMonotonicityWithResolution fnname fn = testProperty (concat ["monotonicity of ", fnname, " with resolution"]) $ do
    either (const False) id actualResultE
    where resolutions = [0..15]
          listIsMonotonicallyDecreasing as = and $ map (\(prev, next) -> prev > next) (zip (init as) (tail as))
          actualResultE = listIsMonotonicallyDecreasing <$> mapM fn resolutions

testCellAreaConversion :: Test
testCellAreaConversion = testProperty "unit conversion from cellAreaKm2 to cellAreaM2" $ \(GenLatLng latLng) (Resolution res) ->
    let h3indexE = latLngToCell latLng res
        convertKm2ToM2 = (*) (1000.0 ** 2)
        actualResultE = h3indexE >>= (fmap convertKm2ToM2 . cellAreaKm2)
        expectedResultE = h3indexE >>= cellAreaM2
        tol = 1e-2
        valuesApproximatelyEqual a b = abs (a - b) < tol
        checkCellValue = liftM2 valuesApproximatelyEqual actualResultE expectedResultE 
    in either (const False) id checkCellValue

testHexagonEdgeLengthAvgConversion :: Test
testHexagonEdgeLengthAvgConversion = testProperty "unit conversion from getHexagonEdgeLengthAvgKm to getHexagonEdgeLengthAvgM" $ do
    either (const False) and checkResolutionValues
    where resolutions = [0..15]
          convertKmToM = (*) 1000.0
          actualResultE = mapM (fmap convertKmToM . getHexagonEdgeLengthAvgKm) resolutions
          expectedResultE = mapM getHexagonEdgeLengthAvgM resolutions
          tol = 1e-6
          listsApproximatelyEqual as bs = zipWith (\a b -> abs (a - b) < tol) as bs
          checkResolutionValues = liftM2 listsApproximatelyEqual actualResultE expectedResultE 

testGetNumCells :: Test
testGetNumCells = testProperty "getNumCells returns number of cells specified in document" $ do
    either (const False) id checkValuesE
    where resolutions = [0..15]
          expectedNumCells res = 2 + 120 * (7 ^ res)
          expectedResultE = Right (map expectedNumCells resolutions)
          actualResultE = mapM getNumCells resolutions
          checkValuesE = liftM2 (==) actualResultE expectedResultE 

testGreatCircleDistanceConversion :: Test
testGreatCircleDistanceConversion = testProperty "unit conversion from greatCircleDistanceKm to greatCircleDistanceM" $ \(GenLatLng latLng1) (GenLatLng latLng2) ->
    let actualResultE = 1000.0 * (greatCircleDistanceKm latLng1 latLng2)
        expectedResultE = greatCircleDistanceM latLng1 latLng2
        tol = eps
        valuesApproximatelyEqual a b = abs (a - b) < tol
    in valuesApproximatelyEqual actualResultE expectedResultE

testGreatCircleRadsNonzero :: Test
testGreatCircleRadsNonzero = testProperty "check coords are equal iff distance is 0" $ \(GenLatLng latLng1) (GenLatLng latLng2) ->
    let diffRads = greatCircleDistanceRads latLng1 latLng2
    in (latLng1 /= latLng2) == (diffRads > 0)

