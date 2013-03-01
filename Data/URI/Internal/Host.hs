{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , OverloadedStrings
  , UnicodeSyntax
  #-}
module Data.URI.Internal.Host
    ( Host
    , parser
    )
    where
import qualified Codec.URI.PercentEncoding as PE
import Control.Applicative
import Control.DeepSeq
import qualified Data.Attoparsec as B
import Data.Attoparsec.Char8
import Data.CaseInsensitive as CI
import Data.Hashable
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
         , pIPv4Addr
         , pRegName
         ]

pIPLiteral ∷ Parser Host
{-# INLINEABLE pIPLiteral #-}
pIPLiteral = (char '[' *> (pIPv6Addr <|> pIPvFuture) <* char ']')
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

{-
      IPv6addrz   = IPv6address "%25" ZoneID

      ZoneID      = 1*( unreserved / pct-encoded )

      IPv6address =                            6( h16 ":" ) ls32
                  /                       "::" 5( h16 ":" ) ls32
                  / [               h16 ] "::" 4( h16 ":" ) ls32
                  / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
                  / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
                  / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
                  / [ *4( h16 ":" ) h16 ] "::"              ls32
                  / [ *5( h16 ":" ) h16 ] "::"              h16
                  / [ *6( h16 ":" ) h16 ] "::"

      ls32        = ( h16 ":" h16 ) / IPv4address
                  ; least-significant 32 bits of address

      h16         = 1*4HEXDIG
                  ; 16 bits of address represented in hexadecimal
-}
pIPv6Addr ∷ Parser Host
{-# INLINEABLE pIPv6Addr #-}
pIPv6Addr = error "FIXME"

pIPv4Addr ∷ Parser Host
{-# INLINEABLE pIPv4Addr #-}
pIPv4Addr = do o0 ← decOctet
               _  ← char '.'
               o1 ← decOctet
               _  ← char '.'
               o2 ← decOctet
               _  ← char '.'
               o3 ← decOctet
               pure $ IPv4Address $ GV.fromList [o0, o1, o2, o3]
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
