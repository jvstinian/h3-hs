module DirectedEdgesTest
    ( tests
    ) where

import Control.Monad (liftM2, join)
import H3.Indexing 
  ( latLngToCell
  , H3ErrorCodes(E_PENTAGON)
  )
import H3.Traversal
  ( gridRingUnsafe
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

tests :: [Test]
tests =
    [ testGroup "Basic functionality check"
        [ testDirectedEdgeToBoundaryHasAtLeastTwoPoints
        ]
    , testGroup "Check identities"
        [ test1RingCellsAreNeighbors
        , test1RingCellsBuildValidDirectEdges 
        , testDirectedEdgesAreValid
        , test1RingCellsToEdgeAndBack 
        , test1RingCellsToEdgeToCells 
        ]
    ]

test1RingCellsAreNeighbors :: Test
test1RingCellsAreNeighbors = testProperty "Testing cells in 1-ring are neighbors" $ \(GenLatLng latLng) (Resolution res) ->
    let h3indexE = latLngToCell latLng res
        ringE = h3indexE >>= flip gridRingUnsafe 1
        cellsAreNeighbors origin neighbors = and <$> (mapM (\neighbor -> areNeighborCells origin neighbor) neighbors)
        resultE = join $ liftM2 cellsAreNeighbors h3indexE ringE
    in (ringE == Left E_PENTAGON) || (resultE == Right True)

test1RingCellsBuildValidDirectEdges :: Test
test1RingCellsBuildValidDirectEdges = testProperty "Testing cells in 1-ring can be used to build valid edges" $ \(GenLatLng latLng) (Resolution res) ->
    let h3indexE = latLngToCell latLng res
        ringE = h3indexE >>= flip gridRingUnsafe 1
        buildDirectedCells origin neighbors = mapM (\neighbor -> cellsToDirectedEdge origin neighbor) neighbors
        directedCellsE = join $ liftM2 buildDirectedCells h3indexE ringE
        resultE = map isValidDirectedEdge <$> directedCellsE
        checkValues = either (const False) and resultE
    in (ringE == Left E_PENTAGON) || checkValues

-- It appears that originToDirectedEdges can produce 0s and so we filter those here before testing validity
testDirectedEdgesAreValid :: Test
testDirectedEdgesAreValid = testProperty "Testing originToDirectedEdges produces valid edges" $ \(GenLatLng latLng) (Resolution res) ->
    let h3indexE = latLngToCell latLng res
        directedEdgesE = filter (/=0) <$> (h3indexE >>= originToDirectedEdges)
        resultE = map isValidDirectedEdge <$> directedEdgesE
        checkValues = either (const False) and resultE
    in checkValues

test1RingCellsToEdgeAndBack :: Test
test1RingCellsToEdgeAndBack = testProperty "Testing cellsToDirectedEdge followed by getDirectedEdgeOrigin and getDirectedEdgeDestination returns original cells" $ \(GenLatLng latLng) (Resolution res) ->
    let h3indexE = latLngToCell latLng res
        ringE = h3indexE >>= flip gridRingUnsafe 1
        buildDirectedCells origin neighbors = mapM (\neighbor -> cellsToDirectedEdge origin neighbor) neighbors
        directedCellsE = join $ liftM2 buildDirectedCells h3indexE ringE
        originsE = directedCellsE >>= mapM getDirectedEdgeOrigin
        destsE = directedCellsE >>= mapM getDirectedEdgeDestination
        checkOriginE = liftM2 (\expt actls -> all (==expt) actls) h3indexE originsE
        checkDestE = liftM2 (==) ringE destsE
        checkOrigins = either (const False) id checkOriginE
        checkDests = either (const False) id checkDestE
    in (ringE == Left E_PENTAGON) || (checkOrigins && checkDests)

test1RingCellsToEdgeToCells :: Test
test1RingCellsToEdgeToCells = testProperty "Testing cellsToDirectedEdge followed by directedEdgeToCells returns original cells" $ \(GenLatLng latLng) (Resolution res) ->
    let h3indexE = latLngToCell latLng res
        ringE = h3indexE >>= flip gridRingUnsafe 1
        buildDirectedCells origin neighbors = mapM (\neighbor -> cellsToDirectedEdge origin neighbor) neighbors
        directedCellsE = join $ liftM2 buildDirectedCells h3indexE ringE
        outputCellsE = directedCellsE >>= mapM directedEdgeToCells
        originsE = map head <$> outputCellsE
        destsE = map last <$> outputCellsE
        outputCellLengthsE = (map length) <$> outputCellsE 
        checkOriginE = liftM2 (\expt actls -> all (==expt) actls) h3indexE originsE
        checkDestE = liftM2 (==) ringE destsE
        checkLengths = either (const False) (all (==2)) outputCellLengthsE
        checkOrigins = either (const False) id checkOriginE
        checkDests = either (const False) id checkDestE
    in (ringE == Left E_PENTAGON) || (checkLengths && checkOrigins && checkDests)

-- It appears that directedEdgeToBoundary can return more than two points, for instance 
-- with LatLng (-0.2265899477443667) (2.6202473683221132) and resolution 3
-- This is also observed in python.
testDirectedEdgeToBoundaryHasAtLeastTwoPoints :: Test
testDirectedEdgeToBoundaryHasAtLeastTwoPoints = testProperty "Testing directedEdgeToBoundary returns at least two points" $ \(GenLatLng latLng) (Resolution res) ->
    let h3indexE = latLngToCell latLng res
        edgeE = head <$> (h3indexE >>= originToDirectedEdges)
        bdryE = edgeE >>= directedEdgeToBoundary
    in (edgeE == Right 0) || either (const False) ((>=2) . length) bdryE

