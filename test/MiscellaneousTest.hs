module MiscellaneousTest
    ( tests
    ) where

import H3.Miscellaneous (degsToRads)

import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      ()

tests :: [Test]
tests =
    [ testGroup "Check conversion from degrees to radians"
        [ checkDegreesToRadiansConversion
        ]
    ]

eps :: RealFloat a => a
eps = eps'
    where
        eps' = encodeFloat 1 (1 - floatDigits eps')

checkDegreesToRadiansConversion :: Test
checkDegreesToRadiansConversion = testProperty "degrees to radians conversion" $ \degs ->
    let expectedRads = degs * pi / 180
        actualRads = degsToRads degs
        tol = 16 * eps
    in abs (expectedRads - actualRads) < tol
        

