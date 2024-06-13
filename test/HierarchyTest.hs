module HierarchyTest
    ( tests
    ) where

import Control.Monad (liftM2)
import Data.List (nub)
import Data.Either (isRight)
import H3.Indexing 
  ( latLngToCell
  )
import H3.Hierarchy
  ( cellToParent 
  , cellToCenterChild 
  , cellToChildPos
  , childPosToCell
  , cellToChildren
  , compactCells
  , uncompactCells
  )
import TestTypes (GenLatLng(GenLatLng, fromGenLatLng), Resolution(Resolution))
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (NonNegative(NonNegative), (==>))

tests :: [Test]
tests =
    [ testGroup "Basic functionality check"
        [ testCompactCellsSucceeds 
        , testUncompactCellsSucceeds 
        ]
    , testGroup "Check identities"
        [ testCellToCenterChildBackToParent 
        , testCellToChildrenBackToParent 
        , testChildPosToCellBackToPos 
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

-- We place an additional restriction on child and parent resolutions in the following 
testCellToChildrenBackToParent :: Test
testCellToChildrenBackToParent = testProperty "Testing cellToChildren followed by cellToParent returns original cells" $ \(GenLatLng latLng) (Resolution res1) (Resolution res2) ->
    let parentRes = min res1 res2
        childRes = max res1 res2
        expectedParentIndexE = latLngToCell latLng parentRes
        childrenIndexesE = expectedParentIndexE >>= flip cellToChildren childRes
        actualParentsIndexE = childrenIndexesE >>= mapM (flip cellToParent parentRes)
        checkResultE = liftM2 (\expParent actParents -> all (==expParent) actParents) expectedParentIndexE actualParentsIndexE
    in res1 /= res2 ==> childRes - parentRes < 6 ==> either (const False) id checkResultE 

testChildPosToCellBackToPos :: Test
testChildPosToCellBackToPos = testProperty "Testing childPosToCell followed by cellToChildPos" $ \(GenLatLng latLng) (Resolution res1) (Resolution res2) (NonNegative childPos) ->
    let parentRes = min res1 res2
        childRes = max res1 res2
        parentIndexE = latLngToCell latLng parentRes
        childrenSizeE = length <$> (parentIndexE >>= flip cellToChildren childRes)
        checkPos = either (const False) (\size -> (fromIntegral childPos) < size) childrenSizeE
        expectedChildPosE = Right childPos
        childIndexE = parentIndexE >>= (\parent -> childPosToCell childPos parent childRes)
        actualChildPosE = childIndexE >>= flip cellToChildPos parentRes
    in res1 /= res2 ==> childRes - parentRes < 6 ==> checkPos ==> actualChildPosE == expectedChildPosE

testCompactCellsSucceeds :: Test
testCompactCellsSucceeds = testProperty "Testing compactCells returns successfully" $ \genLatLngs (Resolution res) ->
    let latLngs = map fromGenLatLng genLatLngs
        cellSetE = mapM (flip latLngToCell res) latLngs
        cellSetDedupE = nub <$> cellSetE -- deduplicate the list of cells, otherwise we will get E_DUPLICATE_INPUT
        resultE = cellSetDedupE >>= compactCells 
    in isRight resultE

testUncompactCellsSucceeds :: Test
testUncompactCellsSucceeds = testProperty "Testing uncompactCells returns successfully" $ \genLatLngs (Resolution res1) (Resolution res2) -> 
    let parentRes = min res1 res2
        childRes = max res1 res2
        latLngs = map fromGenLatLng genLatLngs
        cellSetE = mapM (flip latLngToCell parentRes) latLngs
        resultE = cellSetE >>= flip uncompactCells childRes
    in res1 /= res2 ==> childRes - parentRes < 6 ==> isRight resultE

