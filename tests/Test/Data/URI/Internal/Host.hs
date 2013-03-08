{-# LANGUAGE
    OverloadedStrings
  , UnicodeSyntax
  #-}
module Test.Data.URI.Internal.Host (tests)
    where
import qualified Blaze.ByteString.Builder as BB
import Data.URI.Internal.Host
import qualified Data.Vector.Generic as GV
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit ((@=?))

tests ∷ [Test]
tests = [ testGroup "parser"
          [ testCase "example IPv4address"
            ( Right (IPv4Address (GV.fromList [1,2,3,4]))
              @=?
              (fromByteString "1.2.3.4" ∷ Either String Host)
            )
          , testCase "example IPv6address"
            ( Right (IPv6Address (GV.fromList [0,0,0,0,0,0,0,1]) Nothing)
              @=?
              (fromByteString "[::1]" ∷ Either String Host)
            )
          , testCase "example v4-mapped IPv6address"
            ( Right (IPv6Address (GV.fromList [0,0,0,0,0,0xFFFF,0x7F00,0x0001]) Nothing)
              @=?
              (fromByteString "[::ffff:127.0.0.1]" ∷ Either String Host)
            )
          , testCase "example reg-name"
            ( Right (RegName "cielonegro.org")
              @=?
              (fromByteString "cielonegro.org" ∷ Either String Host)
            )
          ]
        , testGroup "builder"
          [ testCase "example IPv4address"
            ( "1.2.3.4"
              @=?
              BB.toByteString (toBuilder (IPv4Address (GV.fromList [1,2,3,4])))
            )
          , testCase "example IPv6address"
            ( "[::1]"
              @=?
              BB.toByteString (toBuilder (IPv6Address (GV.fromList [0,0,0,0,0,0,0,1]) Nothing))
            )
          , testCase "example v4-mapped IPv6address"
            ( "[::ffff:127.0.0.1]"
              @=?
              BB.toByteString (toBuilder (IPv6Address (GV.fromList [0,0,0,0,0,0xFFFF,0x7F00,0x0001]) Nothing))
            )
          ]
        ]
