module Main (main) where

import Test.Framework (defaultMain, testGroup)
import qualified IndexingTest (tests)
import qualified InspectionTest (tests)
import qualified RegionsTest (tests)
import qualified MiscellaneousTest (tests)
import qualified TraversalTest (tests)

main :: IO ()
main = defaultMain 
    [ testGroup "Indexing" IndexingTest.tests
    , testGroup "Inspection" InspectionTest.tests
    , testGroup "Regions" RegionsTest.tests
    , testGroup "Miscellaneous" MiscellaneousTest.tests
    , testGroup "Traversal" TraversalTest.tests
    ]
