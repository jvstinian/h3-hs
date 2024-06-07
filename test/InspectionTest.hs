module InspectionTest
    ( tests
    ) where

import Data.List (sort)
import H3.Indexing 
  ( latLngToCell )
import H3.Inspection 
  ( h3ToString
  , stringToH3
  , getResolution
  , getBaseCellNumber
  , isValidCell
  , isResClassIII
  , isPentagon
  , getIcosahedronFaces )
import TestTypes 
  ( GenLatLng(GenLatLng)
  , Resolution(Resolution) )
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)


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
        , testIntToStringToInt
        , testIsValidWithMockData
        , testGetIcosahedronFacesWithMockData 
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

testIntToString :: Test
testIntToString = testProperty "Testing conversion from H3 index to cell address" $
    actualValE == expectedValE
    where
      actualValE = h3ToString 599686042433355775 
      expectedValE = Right "85283473fffffff"

testStringToInt :: Test
testStringToInt = testProperty "Testing conversion from cell address to H3 index" $
    actualValE == expectedValE
    where
      actualValE = stringToH3 "85283473fffffff"
      expectedValE = Right 599686042433355775

testGetResolution :: Test
testGetResolution = testProperty "test resulution matches value used to get H3 index" $ \(GenLatLng latLng) (Resolution res) ->
    let actualResE = getResolution <$> latLngToCell latLng res
        expectedResE = Right res
    in 
    actualResE == expectedResE

testIntToStringToInt :: Test
testIntToStringToInt = testProperty "test conversion from int to string and back to int" $ \(GenLatLng latLng) (Resolution res) ->
    let h3indexE = latLngToCell latLng res
        actualResultE = h3indexE >>= h3ToString >>= stringToH3 
        expectedResultE = h3indexE
    in 
    actualResultE == expectedResultE

testIsValidWithMockData :: Test
testIsValidWithMockData = testProperty "test isValidCell" $ \(GenLatLng latLng) (Resolution res) ->
    let actualResultE = (/=0) . isValidCell <$> latLngToCell latLng res
        expectedResultE = Right True
    in 
    actualResultE == expectedResultE

testGetIcosahedronFacesWithMockData :: Test
testGetIcosahedronFacesWithMockData = testProperty "test getIcosahedronFaces" $ \(GenLatLng latLng) (Resolution res) ->
    let resultE = latLngToCell latLng res >>= getIcosahedronFaces >>= (return . checkList)
        checkList = all (\val -> (-1) <= val && val <= 19)
    in 
    either (const False) id resultE

