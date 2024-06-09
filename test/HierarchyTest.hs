module HierarchyTest
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
import H3.Hierarchy
  ( cellToParent 
  , cellToCenterChild 
  , cellToChildPos
  , childPosToCell
  , cellToChildren
  , compactCells
  , uncompactCells
  , uncompactCellsUsingSize
  )
import TestTypes (GenLatLng(GenLatLng), Resolution(Resolution))
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (NonNegative(NonNegative), (==>))

tests :: [Test]
tests =
    [ testGroup "Basic functionality check"
        [
        ]
    , testGroup "Check identities"
        [ testCellToCenterChildBackToParent 
        ]
    ]

testCellToCenterChildBackToParent :: Test
testCellToCenterChildBackToParent = testProperty "Testing cellToCenterChild followed by cellToParent returns original cell" $ \(GenLatLng latLng) (Resolution res1) (Resolution res2) ->
    let parentRes = min res1 res2
        childRes = max res1 res2
        expectedParentIndexE = latLngToCell latLng parentRes
        childIndexE = expectedParentIndexE >>= flip cellToCenterChild childRes
        actualParentIndexE = childIndexE >>= flip cellToParent parentRes
    in res1 /= res2 ==> expectedParentIndexE == actualParentIndexE

-- TODO: Add the following test cases
--
-- For cellToChildren, check that applying cellToParent returns to the original cell
--
-- Apply cellToChildPos followed by childPosToCell and check if the original child cell is recovered
--
-- Perform a basic check for the following
-- compactCells
-- uncompactCells
-- uncompactCellsUsingSize
--
-- For uncompactCells, create a list of input cells using compactCells
--
