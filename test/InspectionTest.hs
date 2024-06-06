module InspectionTest
    ( tests
    ) where

import Control.Monad (liftM2)
import Data.List (sort)
-- import Data.Either (isRight)
import H3.Indexing 
  ( LatLng(LatLng)
  , latLngToCell
  , H3ErrorCodes(E_CELL_INVALID)
  {-, cellToLatLng-} )
import H3.Inspection 
  ( h3ToString
  , stringToH3
  , getResolution
  , getBaseCellNumber
  , isValidCell
  , isResClassIII
  , isPentagon
  , getIcosahedronFaces )
-- import H3.Miscellaneous (degsToRads, radsToDegs)

import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck                      (Arbitrary (..), chooseInt, choose)

tests :: [Test]
tests =
    [ testGroup "Known values for inspection methods"
        [ testGetResolutionWithKnownValue 
        , testGetBaseCellNumberWithKnownValue
        , testIsInvalidCell
        , testIsValidCell
        , testIsResClassIII
        , testIsPentagon
        , testGetIcosahedronFaces
        ]
    , testGroup "Convert between H3Index and Cell Address"
        [ testIntToString
        , testStringToInt
        ]
    , testGroup "Testing inspection methods with mock values"
        [ testGetResolution
        ]
    ]

-- The following was taken from https://github.com/uber/h3/blob/master/tests/cli/getResolution.txt
testGetResolutionWithKnownValue :: Test
testGetResolutionWithKnownValue = testProperty "Testing getResolution" $
    actualResE == expectedResE
    where
        actualResE = getResolution <$> stringToH3 "85283473fffffff"
        expectedResE = Right 5

-- The following was taken from https://github.com/uber/h3/blob/master/tests/cli/getBaseCellNumber.txt
testGetBaseCellNumberWithKnownValue :: Test
testGetBaseCellNumberWithKnownValue = testProperty "Testing getBaseCellNumber" $
    actualResE == expectedResE
    where
        actualResE = getBaseCellNumber <$> stringToH3 "85283473fffffff"
        expectedResE = Right 20

-- The following is from https://github.com/uber/h3/blob/master/tests/cli/isValidCell.txt
testIsInvalidCell :: Test
testIsInvalidCell = testProperty "Testing invalid cell value" $
    actualResultE == expectedResultE
    where
        actualResultE = (/=0) . isValidCell <$> (stringToH3 "85283473ffff")
        expectedResultE = Right False

-- The following is from https://github.com/uber/h3/blob/master/tests/cli/isValidCell.txt
testIsValidCell :: Test
testIsValidCell = testProperty "Testing cell value is valid" $
    actualResultE == expectedResultE
    where
        actualResultE = (/=0) . isValidCell <$> (stringToH3 "85283473fffffff")
        expectedResultE = Right True

testIsResClassIII :: Test
testIsResClassIII = testProperty "Testing isResClassIII" $
    actualResultE == expectedResultE
    where
        actualResultE = (/=0) . isResClassIII <$> (stringToH3 "85283473fffffff")
        expectedResultE = Right True

-- This is taken from https://github.com/uber/h3/blob/master/tests/cli/isPentagon.txt
testIsPentagon :: Test
testIsPentagon = testProperty "Testing isPentagon" $
    actualResultE == expectedResultE
    where
        actualResultE = (/=0) . isPentagon <$> (stringToH3 "85283473fffffff")
        expectedResultE = Right False

-- This is taken from https://github.com/uber/h3/blob/master/tests/cli/getIcosahedronFaces.txt
testGetIcosahedronFaces :: Test
testGetIcosahedronFaces = testProperty "Testing getIcosahedronFaces" $
    actualResultE == expectedResultE
    where
        actualResultE = sort <$> (stringToH3 "81743ffffffffff" >>= getIcosahedronFaces)
        expectedResultE = Right [3, 4, 8, 9, 13]

newtype Resolution = Resolution Int
  deriving (Eq, Show)

instance Arbitrary Resolution where
    arbitrary = Resolution <$> chooseInt (0, 15)

newtype GenLatLng = GenLatLng LatLng
  deriving (Eq, Show)

instance Arbitrary GenLatLng where
    arbitrary = GenLatLng <$> liftM2 LatLng (choose (-pi, pi)) (choose (-pi, pi))

testGetResolution :: Test
testGetResolution = testProperty "test resulution matches value used to get H3 index" $ \(GenLatLng latLng) (Resolution res) ->
    let actualResE = getResolution <$> latLngToCell latLng res
        expectedResE = Right res
    in 
    actualResE == expectedResE

testIntToString :: Test
testIntToString = testProperty "Testing conversion from H3 index to cell address" $
    actualValE == expectedValE
    where
      inputVal = 599686042433355775 
      actualValE = h3ToString inputVal
      expectedValE = Right "85283473fffffff"

testStringToInt :: Test
testStringToInt = testProperty "Testing conversion from cell address to H3 index" $
    actualValE == expectedValE
    where
      inputVal = "85283473fffffff"
      actualValE = stringToH3 inputVal
      expectedValE = Right 599686042433355775

