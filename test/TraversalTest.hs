module TraversalTest
    ( tests
    ) where

import Data.Either (isRight)
import Control.Monad (liftM2, join)
import H3.Miscellaneous (degsToRads)
import H3.Indexing 
  ( LatLng(LatLng)
  , latLngToCell
  , H3ErrorCodes(E_FAILED)
  )
import H3.Inspection
  ( stringToH3
  )
import H3.Traversal
  ( CoordIJ(CoordIJ)
  , gridDisk
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
import Test.QuickCheck                      (Arbitrary (..))

tests :: [Test]
tests =
    [ testGroup "Basic functionality check"
        [ testCellToLocalIj
        , testCellToLocalIjAndBack
        ]
    , testGroup "Adapting CLI tests for Traversal methods"
        [ testCellToLocalIjWithKnownValues
        , testLocalIjToCellWithKnownValues 
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

-- TODO:
-- Port CLI tests for 
-- gridDisk
-- gridDiskDistances
-- gridDistance
-- gridPathCells
-- gridRing

