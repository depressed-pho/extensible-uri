{-# LANGUAGE
    OverloadedStrings
  , UnicodeSyntax
  #-}
module Data.URI.Internal.Host.IPv4
    ( IPv4Addr
    , pIPv4Addr
    , bIPv4Addr
    )
    where
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder.Char8 as BB
import qualified Blaze.Text as BB
import Control.Applicative
import qualified Data.Attoparsec as B
import Data.Attoparsec.Char8
import Data.Monoid.Unicode
import Data.URI.Internal
import Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV
import Data.Word (Word8)
import Prelude.Unicode

type IPv4Addr = UV.Vector Word8

pIPv4Addr ∷ Parser IPv4Addr
{-# INLINEABLE pIPv4Addr #-}
pIPv4Addr = do o0 ← decOctet
               _  ← char '.'
               o1 ← decOctet
               _  ← char '.'
               o2 ← decOctet
               _  ← char '.'
               o3 ← decOctet
               pure $ GV.fromList [o0, o1, o2, o3]
            <?>
            "IPv4address"
    where
      decOctet ∷ Parser Word8
      {-# INLINEABLE decOctet #-}
      decOctet = choice
                 [ -- 250-255
                   do _ ← string "25"
                      x ← atoi <$> B.satisfy (inRange_w8 '0' '5')
                      pure $ 250 + x

                   -- 200-249
                 , do _ ← char '2'
                      x ← atoi <$> B.satisfy (inRange_w8 '0' '4')
                      y ← atoi <$> B.satisfy isDigit_w8
                      pure $ 200 + 10 ⋅ x + y

                   -- 100-199
                 , do _ ← char '1'
                      x ← atoi <$> B.satisfy isDigit_w8
                      y ← atoi <$> B.satisfy isDigit_w8
                      pure $ 100 + 10 ⋅ x + y

                   -- 10-99
                 , do x ← atoi <$> B.satisfy (inRange_w8 '1' '9')
                      y ← atoi <$> B.satisfy isDigit_w8
                      pure $ 10 ⋅ x + y

                   -- 0-9
                 , atoi <$> B.satisfy isDigit_w8
                 ]
                 <?>
                 "dec-octet"

bIPv4Addr ∷ IPv4Addr → Builder
{-# INLINEABLE bIPv4Addr #-}
bIPv4Addr v4
    = BB.integral (v4 ! 0) ⊕
      BB.fromChar '.'      ⊕
      BB.integral (v4 ! 1) ⊕
      BB.fromChar '.'      ⊕
      BB.integral (v4 ! 2) ⊕
      BB.fromChar '.'      ⊕
      BB.integral (v4 ! 3)
