{-# LANGUAGE
    OverloadedStrings
  , UnicodeSyntax
  #-}
module Data.URI.Internal.Host.IPv6
    ( IPv6Addr
    , ZoneID

    , pIPv6Addr
    , pZoneID

    , bIPv6Addr
    , bZoneID
    )
    where
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder.ByteString as BB
import qualified Blaze.ByteString.Builder.Char8 as BB
import qualified Codec.URI.PercentEncoding as PE
import Control.Applicative
import Control.Applicative.Unicode hiding ((∅))
import qualified Data.Attoparsec as B
import Data.Attoparsec.Char8 as C
import Data.Bits
import Data.CaseInsensitive as CI
import qualified Data.List as L
import Data.Monoid.Unicode
import Data.URI.Internal
import Data.URI.Internal.Host.IPv4
import Data.Vector.Generic as GV
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import Data.Vector.Storable.ByteString.Legacy
import qualified Data.Vector.Unboxed as UV
import Data.Word (Word8, Word16)
import Prelude.Unicode

type IPv6Addr = UV.Vector Word16
type ZoneID   = CI ByteString

pIPv6Addr ∷ Parser IPv6Addr
{-# INLINEABLE pIPv6Addr #-}
pIPv6Addr = choice
            [ --                            6( h16 ":" ) ls32
              do x ← countV 6 (h16 <* char ':')
                 y ← ls32
                 pure $ x ⊕ y

            , --                       "::" 5( h16 ":" ) ls32
              do _ ← string "::"
                 x ← countV 5 (h16 <* char ':')
                 y ← ls32
                 pure $ (∅) `pad` x ⊕ y

            , -- [               h16 ] "::" 4( h16 ":" ) ls32
              do x ← option (∅) (singleton <$> h16)
                 _ ← string "::"
                 y ← countV 4 (h16 <* char ':')
                 z ← ls32
                 pure $ x `pad` y ⊕ z

            , -- [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
              do x ← option (∅)
                       (snoc <$> countUpToV 1 (h16 <* char ':') ⊛ h16)
                 _ ← string "::"
                 y ← countV 3 (h16 <* char ':')
                 z ← ls32
                 pure $ x `pad` y ⊕ z

            , -- [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
              do x ← option (∅)
                       (snoc <$> countUpToV 2 (h16 <* char ':') ⊛ h16)
                 _ ← string "::"
                 y ← countV 2 (h16 <* char ':')
                 z ← ls32
                 pure $ x `pad` y ⊕ z

            , -- [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
              do x ← option (∅)
                       (snoc <$> countUpToV 3 (h16 <* char ':') ⊛ h16)
                 _ ← string "::"
                 y ← h16 <* char ':'
                 z ← ls32
                 pure $ x `pad` (y `cons` z)

            , -- [ *4( h16 ":" ) h16 ] "::"              ls32
              do x ← option (∅)
                       (snoc <$> countUpToV 4 (h16 <* char ':') ⊛ h16)
                 _ ← string "::"
                 y ← ls32
                 pure $ x `pad` y

            , -- [ *5( h16 ":" ) h16 ] "::"              h16
              do x ← option (∅)
                       (snoc <$> countUpToV 5 (h16 <* char ':') ⊛ h16)
                 _ ← string "::"
                 y ← h16
                 pure $ x `pad` GV.singleton y

            , -- [ *6( h16 ":" ) h16 ] "::"
              do x ← option (∅)
                       (snoc <$> countUpToV 6 (h16 <* char ':') ⊛ h16)
                 _ ← string "::"
                 pure $ x `pad` (∅)
            ]
            <?>
            "IPv6address"
    where
      h16 ∷ Parser Word16
      {-# INLINEABLE h16 #-}
      h16 = L.foldl' step 0 <$> countUpTo1 4 (B.satisfy isHexDigit_w8)
          where
            step ∷ Word16 → Word8 → Word16
            {-# INLINE step #-}
            step a w = (a `shiftL` 4) .|. htoi w

      ls32 ∷ Parser IPv6Addr
      {-# INLINEABLE ls32 #-}
      ls32 = do x ← h16
                y ← char ':' *> h16
                pure $ GV.fromList [x, y]
             <|>
             (repack <$> pIPv4Addr)
             <?>
             "ls32"

      -- NOTE: This can be implemented using stream operations but I
      -- don't think it's worth it.
      repack ∷ IPv4Addr → IPv6Addr
      {-# INLINEABLE repack #-}
      repack v = GV.fromList
                 [ (fromIntegral (v ! 0) `shiftL` 8) .|. fromIntegral (v ! 1)
                 , (fromIntegral (v ! 2) `shiftL` 8) .|. fromIntegral (v ! 3)
                 ]

      pad ∷ IPv6Addr → IPv6Addr → IPv6Addr
      {-# INLINE pad #-}
      pad x y = let p = GV.replicate (8 - GV.length x - GV.length y) 0
                in
                  x ⊕ p ⊕ y

pZoneID ∷ Parser ZoneID
{-# INLINEABLE pZoneID #-}
pZoneID = do src ← C.takeWhile isAllowed
             case PE.decode' (fromLegacyByteString src) of
               Right dst → pure $ CI.mk dst
               Left  e   → fail $ show (e ∷ PE.DecodeError)
          <?>
          "ZoneID"
    where
      isAllowed ∷ Char → Bool
      {-# INLINE isAllowed #-}
      isAllowed c = isUnreserved c ∨ isPctEncoded c

bIPv6Addr ∷ IPv6Addr → Builder
{-# INLINEABLE bIPv6Addr #-}
bIPv6Addr v6
    -- RFC 5952 recommends IPv4 mixed notation for certain well-known
    -- prefixes.
    | isKnownToBeIPv4Embedded v6 = bIPv4EmbeddedV6Addr v6
    | otherwise                  = bOrdinaryIPv6Addr   v6

isKnownToBeIPv4Embedded ∷ IPv6Addr → Bool
{-# INLINEABLE isKnownToBeIPv4Embedded #-}
isKnownToBeIPv4Embedded v6
    = -- RFC 4291: IPv4-mapped
      prefix96 ≡ GV.fromList [0, 0, 0, 0, 0, 0xFFFF] ∨
      -- RFC 6052: IPv4-translatable
      prefix96 ≡ GV.fromList [0x64, 0xFF9B, 0, 0, 0, 0]
    where
      prefix96 ∷ IPv6Addr
      {-# INLINEABLE prefix96 #-}
      prefix96 = GV.take 6 v6

bIPv4EmbeddedV6Addr ∷ IPv6Addr → Builder
{-# INLINEABLE bIPv4EmbeddedV6Addr #-}
bIPv4EmbeddedV6Addr v6 = bOrdinaryIPv6Addr (GV.take 6 v6) ⊕
                         BB.fromChar ':' ⊕
                         bIPv4Addr (repack (GV.drop 6 v6) ∷ UV.Vector Word8)
    where
      -- NOTE: This can be implemented using stream operations but I
      -- don't think it's worth it.
      repack ∷ IPv6Addr → IPv4Addr
      {-# INLINEABLE repack #-}
      repack v = GV.fromList
                 [ fromIntegral (v ! 0) `shiftR` 8
                 , fromIntegral (v ! 0) .&. 0xFF
                 , fromIntegral (v ! 1) `shiftR` 8
                 , fromIntegral (v ! 1) .&. 0xFF
                 ]

bOrdinaryIPv6Addr ∷ IPv6Addr → Builder
{-# INLINEABLE bOrdinaryIPv6Addr #-}
bOrdinaryIPv6Addr _ = error "FIXME"

bZoneID ∷ Maybe ZoneID → Builder
{-# INLINEABLE bZoneID #-}
bZoneID Nothing  = (∅)
bZoneID (Just z) = BB.fromChar '%' ⊕
                   ( BB.fromByteString               ∘
                     toLegacyByteString              ∘
                     PE.encode' ((¬) ∘ isUnreserved) ∘
                     foldedCase
                   ) z
