module VertexesTest
    ( tests
    ) where

import Data.Either (isRight)
import H3.Indexing (latLngToCell)
import H3.Inspection (isPentagon)
import H3.Vertexes
  ( cellToVertex
  , cellToVertexes 
  , vertexToLatLng
  , isValidVertex
  )
import TestTypes (GenLatLng(GenLatLng), Resolution(Resolution))
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck 
  ( (==>)
  , Arbitrary (..)
  , chooseInt )


newtype GenVertexNum = GenVertexNum Int
  deriving (Eq, Show)

instance Arbitrary GenVertexNum where
    arbitrary = GenVertexNum <$> chooseInt (0, 5)

tests :: [Test]
tests =
    [ testGroup "Basic functionality check"
        [ testCellToVertex
        , testCellToVertexes
        , testVertexToLatLng 
        ]
    ]

testCellToVertex :: Test
testCellToVertex = testProperty "Test cellToVertex produces valid vertex indices" $ \(GenLatLng latLng) (Resolution res) (GenVertexNum vertexNum) ->
    let h3indexE = latLngToCell latLng res
        isPentagonAsBool = (/=0) . isPentagon
        isPent = either (const False) id (isPentagonAsBool <$> h3indexE)
        vertexIndexE = h3indexE >>= flip cellToVertex vertexNum
        resultE = isValidVertex <$> vertexIndexE
    in (not isPent || vertexNum <= 4) ==> (resultE == Right True)

testCellToVertexes :: Test
testCellToVertexes = testProperty "Test cellToVertexes produces valid vertex indices" $ \(GenLatLng latLng) (Resolution res) ->
    let h3indexE = latLngToCell latLng res
        -- isPentagonAsBool = (/=0) . isPentagon
        -- isPent = either (const False) id (isPentagonAsBool <$> h3indexE)
        vertexesE = h3indexE >>= cellToVertexes
        resultE = all isValidVertex <$> vertexesE
    in {- (not isPent || vertexNum <= 4) ==> -} (resultE == Right True)

testVertexToLatLng :: Test
testVertexToLatLng = testProperty "Test cellToVertexes followed by vertexToLatLng returns successfully" $ \(GenLatLng latLng) (Resolution res) ->
    let h3indexE = latLngToCell latLng res
        vertexesE = h3indexE >>= cellToVertexes
        resultE = vertexesE >>= mapM vertexToLatLng
    in isRight resultE

