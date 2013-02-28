{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , UnicodeSyntax
  #-}
module Data.URI.Internal.Host
    ( Host
    )
    where
import Control.Applicative
import Control.DeepSeq
import Data.CaseInsensitive as CI
import Data.Hashable
import Data.Text (Text)
import Data.Typeable
import Data.URI.Internal ()
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import Data.Word (Word8, Word16)
import Numeric.Natural


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
