{-# LANGUAGE
    UnicodeSyntax
  #-}
module Main (main)
    where
import qualified Test.Data.URI.Host as Host
import Test.Framework (Test, defaultMain, testGroup)

main ∷ IO ()
main = defaultMain tests

tests ∷ [Test]
tests = [ testGroup "Data.URI.Host" Host.tests
        ]
