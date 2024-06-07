module MiscellaneousTest
    ( tests
    ) where

import H3.Indexing 
  ( H3Index
  , latLngToCell )
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
        ]
    , testGroup "Check unit conversions"
        [ testHexagonAreaAvgConversion
	, testCellAreaConversion
        ]
    , testGroup "Check monotonicity"
        [ testMonotonicityWithResolution "getHexagonAreaAvgKm2" getHexagonAreaAvgKm2
        , testMonotonicityWithResolution "getHexagonAreaAvgM2" getHexagonAreaAvgM2
        , testMonotonicityWithResolution "getHexagonEdgeLengthAvgKm" getHexagonEdgeLengthAvgKm
        , testMonotonicityWithResolution "getHexagonEdgeLengthAvgM" getHexagonEdgeLengthAvgM
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

{-
testGetHexagonAvgAreaKm2Monotonicity :: Test
testGetHexagonAvgAreaKm2Monotonicity = testProperty "monotonicity of getHexagonAreaAvgKm2 with resolution" $ do
    either (const False) id actualResultE
    where resolutions = [0..15]
          listIsMonotonicallyDecreasing as = and $ map (\(prev, next) -> prev > next) (zip (init as) (tail as))
          actualResultE = listIsMonotonicallyDecreasing <$> mapM getHexagonAreaAvgKm2 resolutions

testGetHexagonAreaAvgKm2Monotonicity :: Test
testGetHexagonAreaAvgKm2Monotonicity = testMonotonicityWithResolution "getHexagonAreaAvgKm2" getHexagonAreaAvgKm2
-}

{-
testMonotonicityForLatLngWithResolution :: (Ord b) => String -> (H3Index -> Either err b) -> Test
testMonotonicityForLatLngWithResolution fnname fn = testProperty (concat ["monotonicity of ", fnname, " with resolution at random coordinates"]) $ \(GenLatLng latLng) (Resolution res) ->
    let h3indexE = latLngToCell latLng res
	fnappl x = h3indexE >>= flip fn x
        listIsMonotonicallyDecreasing as = and $ map (\(prev, next) -> prev > next) (zip (init as) (tail as))
        actualResultE = listIsMonotonicallyDecreasing <$> mapM fnappl resolutions
    in either (const False) id actualResultE
-}

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

-- TODO: Set up tests of the following
-- Check that the conversion from km2 to m2 is satisfied by 
-- getHexagonEdgeLengthAvgKm :: Int -> Either H3ErrorCodes Double
-- and 
-- getHexagonEdgeLengthAvgM :: Int -> Either H3ErrorCodes Double
--
-- For generated LatLng (converted to H3Index), check that 
-- the conversion from km to m is satisfied by  
-- edgeLengthKm :: H3Index -> Either H3ErrorCodes Double
-- and 
-- edgeLengthM :: H3Index -> Either H3ErrorCodes Double
-- 
-- For generated LatLng (converted to H3Index), check that 
-- edgeLengthKm :: H3Index -> Either H3ErrorCodes Double
-- decreases with resolution
-- 
-- Verify the closed formula for 
-- getNumCells :: Int -> Either H3ErrorCodes Int64
--
-- For generated lat-long pairs, check the conversion between 
-- greatCircleDistanceKm 
-- and 
-- greatCircleDistanceM
--
-- Check that 
-- greatCircleDistanceRads
-- is greater than 0 when the coordinates differ

