{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , FlexibleContexts
  , OverloadedStrings
  , ScopedTypeVariables
  , UnicodeSyntax
  #-}
{-# LANGUAGE StandaloneDeriving #-}
module Data.URI.Internal.Host
    ( Host(..)
    , parser
    , fromByteString
    , toBuilder
    )
    where
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder.Char8 as BB
import qualified Codec.URI.PercentEncoding as PE
import Control.Applicative
import Control.Applicative.Unicode hiding ((∅))
import Control.DeepSeq
import Control.Failure
import Data.Attoparsec.Char8 as C
import Data.CaseInsensitive as CI
import Data.Hashable
import Data.Monoid.Unicode
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Typeable
import Data.URI.Internal
import Data.URI.Internal.Host.IPv4
import Data.URI.Internal.Host.IPv6
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import Data.Vector.Storable.ByteString.Legacy
import Numeric.Natural
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
      IPv4Address !IPv4Addr
      -- |8 * 16-bit literal IPv6 address with optional zone ID. (See
      -- <http://tools.ietf.org/html/rfc6874>)
    | IPv6Address !IPv6Addr !(Maybe ZoneID)
      -- |As-yet-undefined IP literal address.
    | IPvFuture   !Natural !(CI ByteString)
      -- |Registered name, which is usually intended for lookup within
      -- a locally defined host or service name registry, though the
      -- URI's scheme-specific semantics may require that a specific
      -- registry (or fixed name table) be used instead.
    | RegName     !(CI Text)
    deriving (Eq, Ord, Typeable)

-- |For testing purpose only.
deriving instance Show Host
--instance Show Host where
--    show = FIXME

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
                lit ← C.takeWhile1 isAllowed
                pure ∘ IPvFuture ver ∘ CI.mk $ fromLegacyByteString lit
             <?>
             "IPvFuture"
    where
      isAllowed ∷ Char → Bool
      {-# INLINEABLE isAllowed #-}
      isAllowed c = isUnreserved c ∨ isSubDelim c ∨ c ≡ ':'

pIPv6Addrz ∷ Parser Host
{-# INLINEABLE pIPv6Addrz #-}
pIPv6Addrz = (IPv6Address <$> pIPv6Addr ⊛ optional (char '%' *> pZoneID))
             <?>
             "IPv6addrz"

pRegName ∷ Parser Host
{-# INLINEABLE pRegName #-}
pRegName = do src ← C.takeWhile isAllowed
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

-- |Try to parse a 'Host' from an ascii string.
fromByteString ∷ Failure String f ⇒ ByteString → f Host
{-# INLINE fromByteString #-}
fromByteString = either failure return ∘
                 parseOnly parser      ∘
                 toLegacyByteString

-- |Create a 'Builder' from a 'Host'.
toBuilder ∷ Host → Builder
{-# INLINEABLE toBuilder #-}
toBuilder (IPv4Address v4  ) = bIPv4Addr v4
toBuilder (IPv6Address v6 z) = BB.fromChar '[' ⊕
                               bIPv6Addr v6    ⊕
                               bZoneID z       ⊕
                               BB.fromChar ']'
