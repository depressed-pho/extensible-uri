{-# LANGUAGE
    DeriveDataTypeable
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
  , UnicodeSyntax
  #-}
module Data.URI.Internal.UserInfo
    ( UserInfo
    )
    where
import Data.ByteString.Char8 (ByteString)
import Data.Data
import Data.Hashable
import Data.Monoid
import Data.Monoid.Unicode
import Data.Semigroup
import Data.String

-- userinfo    = *( unreserved / pct-encoded / sub-delims / ":" )

-- |The userinfo subcomponent may consist of a user name and,
-- optionally, scheme-specific information about how to gain
-- authorization to access the resource. See:
-- <http://tools.ietf.org/html/rfc3986#section-3.2.1>
newtype UserInfo = UserInfo ByteString
    deriving ( Data
             , Eq
             , Hashable
             , Ord
             , Show
             , Typeable
             , Monoid
             )

-- |'fromString' is a fast but unsafe way to create 'UserInfo' such
-- that no validation on the string is performed.
deriving instance IsString UserInfo

instance Semigroup UserInfo where
    {-# INLINE CONLIKE (<>) #-}
    (<>) = (⊕)
