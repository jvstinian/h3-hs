module MiscellaneousTest
    ( tests
    ) where

import H3.Miscellaneous 
  ( degsToRads
  , radsToDegs
  )

import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      ()

tests :: [Test]
tests =
    [ testGroup "Check conversion between degrees and radians"
        [ checkDegreesToRadiansConversion
        , checkRadiansToDegreesConversion
        , checkDegreesConversionRoundtrip
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

checkRadiansToDegreesConversion :: Test
checkRadiansToDegreesConversion = testProperty "radians to degrees conversion" $ \rads ->
    let expectedDegs = rads * 180 / pi
        actualDegs = radsToDegs rads
        tol = 8192 * eps
    in abs (expectedDegs - actualDegs) < tol
 
checkDegreesConversionRoundtrip :: Test
checkDegreesConversionRoundtrip = testProperty "degrees to radians conversion" $ \degs ->
    let expected = degs
        actual = (radsToDegs . degsToRads) degs
        tol = 256 * eps
    in abs (expected - actual) < tol

