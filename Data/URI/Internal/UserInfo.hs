{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , StandaloneDeriving
  , TemplateHaskell
  , TypeSynonymInstances
  , UnicodeSyntax
  #-}
module Data.URI.Internal.UserInfo
    ( UserInfo
    )
    where
import Control.Applicative
import qualified Codec.URI.PercentEncoding as PE
import Data.Ascii (Ascii, AsciiBuilder)
import qualified Data.Ascii as A
import Data.Attoparsec.Char8
import Data.ByteString.Char8 (ByteString)
import Data.Convertible.Base
import Data.Convertible.Instances.Ascii ()
import Data.Data
import Data.Default
import Data.Hashable
import Data.Monoid
import Data.Monoid.Unicode
import Data.Semigroup
import Data.String
import Data.URI.Internal
import Prelude hiding (takeWhile)
import Prelude.Unicode
#if defined(MIN_VERSION_QuickCheck)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Instances ()
#endif

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
#if defined(MIN_VERSION_QuickCheck)
             , Arbitrary
             , CoArbitrary
#endif
             )

-- |'fromString' is a fast but unsafe way to create 'UserInfo' such
-- that no validation on the string is performed.
deriving instance IsString UserInfo

instance Semigroup UserInfo where
    {-# INLINE CONLIKE (<>) #-}
    (<>) = (⊕)

-- |'Parser' for 'UserInfo' which may fail after consuming arbitrary
-- number of input letters.
instance Default (Parser UserInfo) where
    {-# INLINEABLE def #-}
    def = do src ← takeWhile $ \c → isAllowedInUserInfo c ∨ isPctEncoded c
             case PE.decode $ A.unsafeFromByteString src of
               Right dst → pure $ UserInfo dst
               Left  e   → fail $ show (e ∷ PE.DecodingFailed)
          <?>
          "userinfo"

isAllowedInUserInfo ∷ Char → Bool
isAllowedInUserInfo c
    = isUnreserved c ∨
      isSubDelim   c ∨
      ':' ≡        c

-- |Extract a 'ByteString' from 'UserInfo'.
instance ConvertSuccess UserInfo ByteString where
    {-# INLINE convertSuccess #-}
    convertSuccess (UserInfo ui) = ui

-- |Create an 'AsciiBuilder' from 'UserInfo' with all unsafe letters
-- percent-encoded.
instance ConvertSuccess UserInfo AsciiBuilder where
    {-# INLINE convertSuccess #-}
    convertSuccess = cs ∘ PE.encode ((¬) ∘ isAllowedInUserInfo) ∘ cs

-- |Create an 'UserInfo' from any 'ByteString's.
instance ConvertSuccess ByteString UserInfo where
    {-# INLINE CONLIKE convertSuccess #-}
    convertSuccess = UserInfo

-- |Try to parse an 'UserInfo' from 'Ascii' with decoding all
-- percent-encoded octets.
instance ConvertAttempt Ascii UserInfo where
    {-# INLINE convertAttempt #-}
    convertAttempt = parseAttempt' def

deriveAttempts [ ([t| ByteString |], [t| UserInfo     |])
               , ([t| UserInfo   |], [t| AsciiBuilder |])
               , ([t| UserInfo   |], [t| ByteString   |])
               ]
