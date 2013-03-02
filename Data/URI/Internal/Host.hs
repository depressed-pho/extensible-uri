{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , FlexibleContexts
  , OverloadedStrings
  , ScopedTypeVariables
  , UnicodeSyntax
  #-}
module Data.URI.Internal.Host
    ( Host
    , parser
    )
    where
import qualified Codec.URI.PercentEncoding as PE
import Control.Applicative
import Control.Applicative.Unicode hiding ((∅))
import Control.DeepSeq
import qualified Data.Attoparsec as B
import Data.Attoparsec.Char8
import Data.Bits
import Data.CaseInsensitive as CI
import qualified Data.List as L
import Data.Hashable
import Data.Monoid
import Data.Monoid.Unicode
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Typeable
import Data.URI.Internal
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import Data.Vector.Storable.ByteString.Legacy
import Data.Word (Word8, Word16)
import Numeric.Natural
import Prelude hiding (takeWhile)
import Prelude.Unicode

-- |The 'Host' subcomponent of authority is identified by an IP
-- literal encapsulated within square brackets, an IPv4 address in
-- dotted-decimal form, or a registered name. The host subcomponent is
-- case-insensitive. The presence of a host subcomponent within a URI
-- does not imply that the scheme requires access to the given host on
-- the Internet. In many cases, the host syntax is used only for the
-- sake of reusing the existing registration process created and
-- deployed for DNS, thus obtaining a globally unique name without the
-- cost of deploying another registry. See:
-- <http://tools.ietf.org/html/rfc3986#section-3.2.2>
data Host
    = -- |4-octets literal IPv4 address.
      IPv4Address !(UV.Vector Word8)
      -- |8 * 16-bit literal IPv6 address with optional zone ID. (See
      -- <http://tools.ietf.org/html/rfc6874>)
    | IPv6Address !(UV.Vector Word16) !(Maybe (CI ByteString))
      -- |As-yet-undefined IP literal address.
    | IPvFuture   !Natural !(CI ByteString)
      -- |Registered name, which is usually intended for lookup within
      -- a locally defined host or service name registry, though the
      -- URI's scheme-specific semantics may require that a specific
      -- registry (or fixed name table) be used instead.
    | RegName     !(CI Text)
    deriving (Eq, Ord, Typeable)

instance FoldCase Host where
    {-# INLINEABLE foldCase #-}
    foldCase (IPv4Address w  ) = IPv4Address w
    foldCase (IPv6Address w s) = IPv6Address w (foldCase <$> s)
    foldCase (IPvFuture   v a) = IPvFuture   v (foldCase a)
    foldCase (RegName     n  ) = RegName       (foldCase n)

instance Hashable Host where
    {-# INLINEABLE hashWithSalt #-}
    hashWithSalt salt (IPv4Address w  ) = salt `hashWithSalt` w
    hashWithSalt salt (IPv6Address w s) = salt `hashWithSalt` w `hashWithSalt` s
    hashWithSalt salt (IPvFuture   v a) = salt `hashWithSalt` v `hashWithSalt` a
    hashWithSalt salt (RegName     n  ) = salt `hashWithSalt` n

instance NFData Host where
    {-# INLINEABLE rnf #-}
    rnf (IPv4Address w  ) = rnf w
    rnf (IPv6Address w s) = rnf w `seq` rnf s
    rnf (IPvFuture   v a) = rnf v `seq` rnf a
    rnf (RegName     n  ) = rnf n

-- |'Parser' for 'Host's.
parser ∷ Parser Host
{-# INLINEABLE parser #-}
parser = choice
         [ pIPLiteral
         , IPv4Address <$> pIPv4Addr
         , pRegName
         ]

pIPLiteral ∷ Parser Host
{-# INLINEABLE pIPLiteral #-}
pIPLiteral = (char '[' *> (pIPv6Addrz <|> pIPvFuture) <* char ']')
             <?>
             "IP-literal"

pIPvFuture ∷ Parser Host
{-# INLINEABLE pIPvFuture #-}
pIPvFuture = do _   ← char 'v'
                ver ← hexadecimal
                _   ← char '.'
                lit ← takeWhile1 isAllowed
                pure $ IPvFuture ver $ CI.mk $ fromLegacyByteString lit
             <?>
             "IPvFuture"
    where
      isAllowed ∷ Char → Bool
      {-# INLINEABLE isAllowed #-}
      isAllowed c = isUnreserved c ∨ isSubDelim c ∨ c ≡ ':'

pIPv6Addrz ∷ Parser Host
{-# INLINEABLE pIPv6Addrz #-}
pIPv6Addrz = (IPv6Address <$> pIPv6Addr ⊛ (char '%' *> optional pZoneID))
             <?>
             "IPv6addrz"

pIPv6Addr ∷ ∀v. (GV.Vector v Word16, Monoid (v Word16)) ⇒ Parser (v Word16)
{-# INLINEABLE pIPv6Addr #-}
pIPv6Addr = choice
            [ do x ← countV 6 (h16 <* char ':')
                 y ← ls32
                 pure $ x ⊕ y

            , do _ ← string "::"
                 x ← countV 5 (h16 <* char ':')
                 y ← ls32
                 pure $ (∅) `pad` x ⊕ y

            , do x ← option (∅) (GV.singleton <$> h16)
                 _ ← string "::"
                 y ← countV 4 (h16 <* char ':')
                 z ← ls32
                 pure $ x `pad` y ⊕ z

            , do x ← option (∅)
                       (GV.snoc <$> countUpToV 1 (h16 <* char ':') ⊛ h16)
                 _ ← string "::"
                 y ← countV 3 (h16 <* char ':')
                 z ← ls32
                 pure $ x `pad` y ⊕ z

            , do x ← option (∅)
                       (GV.snoc <$> countUpToV 2 (h16 <* char ':') ⊛ h16)
                 _ ← string "::"
                 y ← countV 2 (h16 <* char ':')
                 z ← ls32
                 pure $ x `pad` y ⊕ z

            , do x ← option (∅)
                       (GV.snoc <$> countUpToV 3 (h16 <* char ':') ⊛ h16)
                 _ ← string "::"
                 y ← h16 <* char ':'
                 z ← ls32
                 pure $ x `pad` (y `GV.cons` z)

            , do x ← option (∅)
                       (GV.snoc <$> countUpToV 4 (h16 <* char ':') ⊛ h16)
                 _ ← string "::"
                 y ← ls32
                 pure $ x `pad` y

            , do x ← option (∅)
                       (GV.snoc <$> countUpToV 5 (h16 <* char ':') ⊛ h16)
                 _ ← string "::"
                 y ← h16
                 pure $ x `pad` GV.singleton y

            , do x ← option (∅)
                       (GV.snoc <$> countUpToV 6 (h16 <* char ':') ⊛ h16)
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

      ls32 ∷ Parser (v Word16)
      {-# INLINEABLE ls32 #-}
      ls32 = do x ← h16
                y ← char ':' *> h16
                pure $ GV.fromList [x, y]
             <|>
             (repack <$> (pIPv4Addr ∷ Parser (UV.Vector Word8)))
             <?>
             "ls32"

      repack ∷ GV.Vector v' Word8 ⇒ v' Word8 → v Word16
      {-# INLINEABLE repack #-}
      repack v = GV.fromList
                 [ (fromIntegral (v GV.! 0) `shiftL` 8) .|. fromIntegral (v GV.! 1)
                 , (fromIntegral (v GV.! 2) `shiftL` 8) .|. fromIntegral (v GV.! 3)
                 ]

      pad ∷ v Word16 → v Word16 → v Word16
      {-# INLINE pad #-}
      pad x y = let p = GV.replicate (8 - GV.length x - GV.length y) 0
                in
                  x ⊕ p ⊕ y

pZoneID ∷ Parser (CI ByteString)
{-# INLINEABLE pZoneID #-}
pZoneID = do src ← takeWhile isAllowed
             case PE.decode' (fromLegacyByteString src) of
               Right dst → pure $ CI.mk dst
               Left  e   → fail $ show (e ∷ PE.DecodeError)
          <?>
          "ZoneID"
    where
      isAllowed ∷ Char → Bool
      {-# INLINE isAllowed #-}
      isAllowed c = isUnreserved c ∨ isPctEncoded c

pIPv4Addr ∷ GV.Vector v Word8 ⇒ Parser (v Word8)
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

pRegName ∷ Parser Host
{-# INLINEABLE pRegName #-}
pRegName = do src ← takeWhile isAllowed
              case PE.decode' (fromLegacyByteString src) of
                Right dst →
                    case T.decodeUtf8' (toLegacyByteString dst) of
                      Right dst' → pure ∘ RegName ∘ CI.mk $ dst'
                      Left  e    → fail $ show e
                Left  e   →
                    fail $ show (e ∷ PE.DecodeError)
           <?>
           "reg-name"
    where
      isAllowed ∷ Char → Bool
      {-# INLINE isAllowed #-}
      isAllowed c = isUnreserved c ∨
                    isPctEncoded c ∨
                    isSubDelim   c
