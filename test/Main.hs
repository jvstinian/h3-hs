module Main (main) where

import Test.Framework (defaultMain, testGroup)
import qualified IndexingTest (tests)
import qualified MiscellaneousTest (tests)

main :: IO ()
main = defaultMain 
    [ testGroup "Indexing" IndexingTest.tests
    , testGroup "Miscellaneous" MiscellaneousTest.tests
    ]
