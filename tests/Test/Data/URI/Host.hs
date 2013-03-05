{-# LANGUAGE
    OverloadedStrings
  , UnicodeSyntax
  #-}
module Test.Data.URI.Host (tests)
    where
import Data.URI.Internal.Host
import qualified Data.Vector.Generic as GV
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

tests ∷ [Test]
tests = [ testCase "example IPv4address"
          ( Right (IPv4Address (GV.fromList [1, 2, 3, 4]))
            @=?
            (fromByteString "1.2.3.4" ∷ Either String Host)
          )
        , testCase "example IPv6address"
          ( Right (IPv6Address (GV.fromList [0, 0, 0, 0, 0, 0, 0, 1]) Nothing)
            @=?
            (fromByteString "[::1]" ∷ Either String Host)
          )
        , testCase "example reg-name"
          ( Right (RegName "cielonegro.org")
            @=?
            (fromByteString "cielonegro.org" ∷ Either String Host)
          )
        ]
