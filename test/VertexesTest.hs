module VertexesTest
    ( tests
    ) where

import Control.Monad (liftM2, join)
import H3.Indexing 
  ( H3Index
  , latLngToCell
  , H3ErrorCodes(E_FAILED, E_PENTAGON)
  )
import H3.Inspection
  ( stringToH3
  , isPentagon
  )
import H3.Traversal
  ( gridRingUnsafe
  )
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
  ( NonNegative(NonNegative)
  , (==>)
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
        ]
    ]

-- TODO: Add the following test cases
--
-- For an arbitrary cell H3 index, use cellToVertex to get a H3 vertex index, and check if it is valid.  
-- As the vertexNum must be in [0..4] for a pentagon, we need some logic for what to do when a pentagon is encountered. - DONE
--
-- For an arbitrary cell H3 index, use cellToVertexes to get the list of H3 vertex indices, 
-- and check they are valid with isValidVertex.
-- 
-- For an arbitrary cell H3 index, use cellToVertexes o cellToVertex to get a vertex index, and then check that vertexToLatLng 
-- successfully returns 
--

testCellToVertex :: Test
testCellToVertex = testProperty "Test cellToVertex produces valid vertex indices" $ \(GenLatLng latLng) (Resolution res) (GenVertexNum vertexNum) ->
    let h3indexE = latLngToCell latLng res
        isPentagonAsBool = (/=0) . isPentagon
        isPent = either (const False) id (isPentagonAsBool <$> h3indexE)
        vertexIndexE = h3indexE >>= flip cellToVertex vertexNum
	resultE = isValidVertex <$> vertexIndexE
    in (not isPent || vertexNum <= 4) ==> (resultE == Right True)

