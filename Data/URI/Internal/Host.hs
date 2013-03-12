{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , FlexibleContexts
  , OverloadedStrings
  , ScopedTypeVariables
  , UnicodeSyntax
  #-}
module Data.URI.Internal.Host
    ( Host(..)
    , IPv4Addr(..)
    , IPv6Addr(..)
    , ZoneID(..)
    , parser
    , fromByteString
    , toBuilder
    )
    where
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as BB
import qualified Blaze.ByteString.Builder.Char8 as BB
import qualified Blaze.Text as BB
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
import qualified Data.Vector.Storable.ByteString.Char8 as C8
import Data.Vector.Storable.ByteString.Legacy
import Numeric.Natural
import Prelude.Unicode
#if defined(MIN_VERSION_QuickCheck)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
#endif

-- |The 'Host' subcomponent of authority is identified by an IP
-- literal encapsulated within square brackets, an IPv4 address in
-- dotted-decimal form, or a registered name. The host subcomponent is
-- case-insensitive. The presence of a host subcomponent within a URI
-- does not imply that the scheme requires access to the given host on
-- the Internet. In many cases, the host subcomponent is used only for
-- the sake of reusing the existing registration process created and
-- deployed for DNS, thus obtaining a globally unique name without the
-- cost of deploying another registry. See:
-- <http://tools.ietf.org/html/rfc3986#section-3.2.2>
data Host
    = -- |4-octets literal IPv4 address.
      IPv4Address !IPv4Addr
      -- |8 * 16-bit literal IPv6 address with optional zone ID. (See
      -- <http://tools.ietf.org/html/rfc6874>)
    | IPv6Address !IPv6Addr !(Maybe (CI ZoneID))
      -- |As-yet-undefined IP literal address.
    | IPvFuture   !Natural !(CI ByteString)
      -- |Registered name, which is usually intended for lookup within
      -- a locally defined host or service name registry, though the
      -- URI's scheme-specific semantics may require that a specific
      -- registry (or fixed name table) be used instead.
    | RegName     !(CI Text)
    deriving (Eq, Ord, Typeable)

-- |For testing purpose only.
instance Show Host where
    show = C8.unpack ∘ fromLegacyByteString ∘ BB.toByteString ∘ toBuilder

instance FoldCase Host where
    {-# INLINEABLE foldCase #-}
    foldCase (IPv4Address a  ) = IPv4Address a
    foldCase (IPv6Address a z) = IPv6Address a (foldCase <$> z)
    foldCase (IPvFuture   v a) = IPvFuture   v (foldCase a)
    foldCase (RegName     n  ) = RegName       (foldCase n)

instance Hashable Host where
    {-# INLINEABLE hashWithSalt #-}
    hashWithSalt salt (IPv4Address a  ) = salt `hashWithSalt` a
    hashWithSalt salt (IPv6Address a z) = salt `hashWithSalt` a `hashWithSalt` z
    hashWithSalt salt (IPvFuture   v a) = salt `hashWithSalt` v `hashWithSalt` a
    hashWithSalt salt (RegName     n  ) = salt `hashWithSalt` n

instance NFData Host where
    {-# INLINEABLE rnf #-}
    rnf (IPv4Address a  ) = rnf a
    rnf (IPv6Address a z) = rnf a `seq` rnf z
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
pIPv6Addrz = (IPv6Address <$> pIPv6Addr ⊛ optional ("%25" .*> (CI.mk <$> pZoneID)))
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

toBuilder (IPv4Address v4)
    = bIPv4Addr v4

toBuilder (IPv6Address v6 z)
    = BB.fromChar '[' ⊕
      bIPv6Addr v6    ⊕
      bMaybeZoneID z  ⊕
      BB.fromChar ']'
    where
      bMaybeZoneID ∷ Maybe (CI ZoneID) → Builder
      {-# INLINEABLE bMaybeZoneID #-}
      bMaybeZoneID = maybe (∅) ((BB.copyByteString "%25" ⊕) ∘ bZoneID ∘ CI.foldedCase)

toBuilder (IPvFuture v lit)
    = BB.fromChar '[' ⊕
      BB.fromChar 'v' ⊕
      BB.integral v   ⊕
      BB.fromChar '.' ⊕
      (BB.fromByteString ∘ toLegacyByteString ∘ foldedCase) lit ⊕
      BB.fromChar ']'

toBuilder (RegName r) = bRegName r
    where
      bRegName ∷ CI Text → Builder
      {-# INLINEABLE bRegName #-}
      bRegName = BB.fromByteString                  ∘
                 toLegacyByteString                 ∘
                 PE.encode' ((¬) ∘ isSafeInRegName) ∘
                 fromLegacyByteString               ∘
                 T.encodeUtf8                       ∘
                 foldedCase

      isSafeInRegName ∷ Char → Bool
      {-# INLINEABLE isSafeInRegName #-}
      isSafeInRegName c = isUnreserved c ∨ isSubDelim c

#if defined(MIN_VERSION_QuickCheck)
instance Arbitrary Host where
    arbitrary = oneof [ IPv4Address <$> arbitrary
                      , IPv6Address <$> arbitrary ⊛ arbitrary
                      , IPvFuture   <$> arbitrary ⊛ arbitrary
                      , RegName     <$> arbitrary
                      ]

    shrink (IPv4Address a  ) = IPv4Address <$> shrink a
    shrink (IPv6Address a z) = [ IPv6Address a' z  | a' ← shrink a ] ⊕
                               [ IPv6Address a  z' | z' ← shrink z ]
    shrink (IPvFuture   v a) = [ IPvFuture   v' a  | v' ← shrink v ] ⊕
                               [ IPvFuture   v  a' | a' ← shrink a ]
    shrink (RegName     n  ) = RegName <$> shrink n

instance CoArbitrary Host where
    coarbitrary (IPv4Address a  ) = coarbitrary a
    coarbitrary (IPv6Address a z) = coarbitrary a >< coarbitrary z
    coarbitrary (IPvFuture   v a) = coarbitrary v >< coarbitrary a
    coarbitrary (RegName     n  ) = coarbitrary n
#endif
