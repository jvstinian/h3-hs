module DirectedEdgesTest
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
  )
import H3.DirectedEdges
  ( isValidDirectedEdge 
  , directedEdgeToCells
  , originToDirectedEdges
  , areNeighborCells
  , cellsToDirectedEdge
  , getDirectedEdgeOrigin
  , getDirectedEdgeDestination
  , directedEdgeToBoundary
  )
import TestTypes (GenLatLng(GenLatLng), Resolution(Resolution))
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (NonNegative(NonNegative), (==>))

tests :: [Test]
tests =
    [ testGroup "Basic functionality check"
        [ testDirectedEdgeToBoundaryHasAtLeastTwoPoints
        ]
    , testGroup "Check identities"
        [ 
        ]
    ]

{-
testCellToCenterChildBackToParent :: Test
testCellToCenterChildBackToParent = testProperty "Testing cellToCenterChild followed by cellToParent returns original cell" $ \(GenLatLng latLng) (Resolution res1) (Resolution res2) ->
    let parentRes = min res1 res2
        childRes = max res1 res2
        expectedParentIndexE = latLngToCell latLng parentRes
        childIndexE = expectedParentIndexE >>= flip cellToCenterChild childRes
        actualParentIndexE = childIndexE >>= flip cellToParent parentRes
    in res1 /= res2 ==> expectedParentIndexE == actualParentIndexE
-}

-- TODO: Add the following test cases
--
-- areNeighborCells could be tested by applying gridRingUnsafe to a random point and checking that the indexes are neighbors
--
-- apply cellsToDirectedEdge to the results of gridRingUnsafe (or originToDirectedEdges) and then run isValidDirectedEdge
--
-- apply cellsToDirectedEdge to the results of gridRingUnsafe and then run getDirectedEdgeOrigin and getDirectedEdgeDestination 
-- to return to the values produced by gridRingUnsafe
--
-- similarly use directedEdgeToCells as an inverse to cellsToDirectedEdge 
--
-- check that cellsToDirectedEdge produces the results of getDirectedEdgeOrigin and getDirectedEdgeDestination 
-- 
-- originToDirectedEdges could be used instead of gridRingUnsafe to produce edges
--
-- for directedEdgeToBoundary, try to identify a reasonable test, and fallback to a simple check of success if necessary
--

-- It appears that directedEdgeToBoundary can return more than two points, for instance 
-- with LatLng (-0.2265899477443667) (2.6202473683221132) and resolution 3
-- This is also observed in python.
testDirectedEdgeToBoundaryHasAtLeastTwoPoints :: Test
testDirectedEdgeToBoundaryHasAtLeastTwoPoints = testProperty "Testing directedEdgeToBoundary returns at least two points" $ \(GenLatLng latLng) (Resolution res) ->
    let h3indexE = latLngToCell latLng res
        edgeE = head <$> (h3indexE >>= originToDirectedEdges)
        bdryE = edgeE >>= directedEdgeToBoundary
    in (edgeE == Right 0) || either (const False) ((>=2) . length) bdryE

