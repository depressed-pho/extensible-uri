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
import Data.CaseInsensitive as CI
import Data.Hashable
import Data.LargeWord (Word128)
import Data.Text (Text)
import Data.Typeable
import Data.URI.Internal ()
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import Data.Word (Word32)


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
    = IPv4Address !Word32
    | IPv6Address !Word128 !(Maybe (CI Text))
    | IPvFuture   !Integer !(CI ByteString)
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
