module TraversalTest
    ( tests
    ) where

import Control.Monad (liftM2, join)
import H3.Data
  ( H3Index
  , H3ErrorCodes(E_FAILED, E_PENTAGON)
  , CoordIJ(CoordIJ)
  )
import H3.Indexing (latLngToCell)
import H3.Inspection (stringToH3)
import H3.Traversal
  ( gridDisk
  , gridDiskUnsafe
  , gridDiskDistances
  , gridDiskDistancesSafe
  , gridDiskDistancesUnsafe
  , gridRingUnsafe
  , gridPathCells
  , gridDistance 
  , cellToLocalIj
  , localIjToCell
  )
import TestTypes (GenLatLng(GenLatLng), Resolution(Resolution))
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (NonNegative(NonNegative), (==>))

maxGridDistance :: Int
maxGridDistance = 10

tests :: [Test]
tests =
    [ testGroup "Basic functionality check"
        [ testCellToLocalIj
        , testCellToLocalIjAndBack
        , testGridDiskMethods "gridDisk" gridDisk
        , testGridDiskMethods "gridDiskUnsafe" gridDiskUnsafe
        , testGridDiskDistancesMethods maxGridDistance "gridDiskDistances" gridDiskDistances
        , testGridDiskDistancesMethods maxGridDistance "gridDiskDistancesUnsafe" gridDiskDistancesUnsafe
        ]
    , testGroup "Adapting CLI tests for Traversal methods"
        [ testCellToLocalIjWithKnownValues
        , testLocalIjToCellWithKnownValues 
        , testGridDiskWithKnownValues 
        , testGridDiskDistancesWithKnownValues
        , testGridDistanceWithKnownValues
        , testGridPathCellsWithKnownValues
        , testGridRingUnsafeWithKnownValues 
        ]
    ]

-- We allow for either success or an error code E_FAILED
testCellToLocalIj :: Test
testCellToLocalIj = testProperty "Testing cellToLocalIj" $ \(GenLatLng latLng1) (GenLatLng latLng2) (Resolution res) ->
    let h3indexE1 = latLngToCell latLng1 res
        h3indexE2 = latLngToCell latLng2 res
        resultE = join $ liftM2 cellToLocalIj h3indexE1 h3indexE2
    in either (==E_FAILED) (const True) resultE

testCellToLocalIjAndBack :: Test
testCellToLocalIjAndBack = testProperty "Testing cellToLocalIj followed by localIjToCell" $ \(GenLatLng latLng1) (GenLatLng latLng2) (Resolution res) ->
    let h3indexE1 = latLngToCell latLng1 res
        expectedH3indexE = latLngToCell latLng2 res
        coordE = join $ liftM2 cellToLocalIj h3indexE1 expectedH3indexE
        actualH3indexE = join $ liftM2 localIjToCell h3indexE1 coordE
        compResultE = liftM2 (==) actualH3indexE expectedH3indexE
    in either (==E_FAILED) id compResultE

testGridDiskMethods :: String -> (H3Index -> Int -> Either H3ErrorCodes [H3Index]) -> Test
testGridDiskMethods fnname fn = testProperty (concat ["Testing ", fnname]) $ \(GenLatLng latLng1) (Resolution res) (NonNegative k) ->
    let h3indexE1 = latLngToCell latLng1 res
        resultE = h3indexE1 >>= flip fn k
    in either (==E_PENTAGON) (const True) resultE

-- Note in the following that the first argument is used to restrict the distance considered
testGridDiskDistancesMethods :: Int -> String -> (H3Index -> Int -> Either H3ErrorCodes ([H3Index], [Int])) -> Test
testGridDiskDistancesMethods maxk fnname fn = testProperty (concat ["Testing ", fnname]) $ \(GenLatLng latLng1) (Resolution res) (NonNegative k) ->
    let h3indexE1 = latLngToCell latLng1 res
        resultE = h3indexE1 >>= flip fn k
    in k <= maxk ==> either (==E_PENTAGON) (const True) resultE

testCellToLocalIjWithKnownValues :: Test
testCellToLocalIjWithKnownValues = testProperty "CLI test for cellToLocalIj " $
    actualResultE == expectedResultE
    where h3indexE1 = stringToH3 "85283473fffffff"
          h3indexE2 = stringToH3 "8528342bfffffff"
          actualResultE = join $ liftM2 cellToLocalIj h3indexE1 h3indexE2
          expectedResultE = Right $ CoordIJ 25 13

-- We allow for either an exact match of the H3 index or an error code E_FAILED
testLocalIjToCellWithKnownValues :: Test
testLocalIjToCellWithKnownValues = testProperty "CLI test for localIjToCell" $
    actualResultE == expectedResultE
    where h3indexE1 = stringToH3 "85283473fffffff"
          coordij = CoordIJ 25 13
          actualResultE = h3indexE1 >>= flip localIjToCell coordij
          expectedResultE = stringToH3 "8528342bfffffff"

testGridDiskWithKnownValues :: Test
testGridDiskWithKnownValues = testProperty "CLI test for gridDisk" $
    actualResultE == expectedResultE
    where h3indexE1 = stringToH3 "85283473fffffff"
          k = 1
          actualResultE = h3indexE1 >>= flip gridDisk k
          expectedStrs = [ "85283473fffffff", "85283447fffffff", "8528347bfffffff"
                         , "85283463fffffff", "85283477fffffff", "8528340ffffffff"
                         , "8528340bfffffff" ]
          expectedResultE = mapM stringToH3 expectedStrs

testGridDiskDistancesWithKnownValues :: Test
testGridDiskDistancesWithKnownValues = testProperty "CLI test for gridDiskDistances" $
    actualResultE == expectedResultE
    where h3indexE1 = stringToH3 "85283473fffffff"
          k = 1
          actualResultE = h3indexE1 >>= flip gridDiskDistances k
          expectedStrs = [ "85283473fffffff", "85283447fffffff", "8528347bfffffff"
                         , "85283463fffffff", "85283477fffffff", "8528340ffffffff"
                         , "8528340bfffffff" ]
          expectedDistances = [0] ++ replicate 6 1
          expectedResultE = mapM stringToH3 expectedStrs >>= (\h3indices -> return (h3indices, expectedDistances))

testGridDistanceWithKnownValues :: Test
testGridDistanceWithKnownValues = testProperty "CLI test for gridDistance" $
    actualResultE == expectedResultE
    where h3indexE1 = stringToH3 "85283473fffffff"
          h3indexE2 =  stringToH3 "8528342bfffffff"
          actualResultE = join $ liftM2 gridDistance h3indexE1 h3indexE2
          expectedResultE = Right 2

testGridPathCellsWithKnownValues :: Test
testGridPathCellsWithKnownValues = testProperty "CLI test for gridPathCells" $
    actualResultE == expectedResultE
    where h3indexE1 = stringToH3 "85283473fffffff"
          h3indexE2 = stringToH3 "8528342bfffffff"
          actualResultE = join $ liftM2 gridPathCells h3indexE1 h3indexE2
          expectedStrs = [ "85283473fffffff", "85283477fffffff", "8528342bfffffff" ]
          expectedResultE = mapM stringToH3 expectedStrs

testGridRingUnsafeWithKnownValues :: Test
testGridRingUnsafeWithKnownValues = testProperty "CLI test for gridRingUnsafe" $
    actualResultE == expectedResultE
    where h3indexE1 = stringToH3 "85283473fffffff"
          k = 1
          actualResultE = h3indexE1 >>= flip gridRingUnsafe k
          expectedStrs = [ "8528340bfffffff", "85283447fffffff", "8528347bfffffff"
                         , "85283463fffffff", "85283477fffffff", "8528340ffffffff" ]
          expectedResultE = mapM stringToH3 expectedStrs

