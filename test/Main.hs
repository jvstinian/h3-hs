module Main (main) where

import Test.Framework (defaultMain, testGroup)
import qualified IndexingTest (tests)
import qualified InspectionTest (tests)
import qualified MiscellaneousTest (tests)

main :: IO ()
main = defaultMain 
    [ testGroup "Indexing" IndexingTest.tests
    , testGroup "Inspection" InspectionTest.tests
    , testGroup "Miscellaneous" MiscellaneousTest.tests
    ]
