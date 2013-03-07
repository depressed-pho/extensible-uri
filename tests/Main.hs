{-# LANGUAGE
    UnicodeSyntax
  #-}
module Main (main)
    where
import qualified Test.Data.URI.Internal.Host as Host
import Test.Framework (Test, defaultMain, testGroup)

main ∷ IO ()
main = defaultMain tests

tests ∷ [Test]
tests = [ testGroup "Data.URI.Internal.Host" Host.tests
        ]
