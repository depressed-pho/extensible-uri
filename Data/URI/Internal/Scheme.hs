{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , StandaloneDeriving
  , UnicodeSyntax
  #-}
module Data.URI.Internal.Scheme
    ( Scheme
    , parser
    , fromByteString
    , toBuilder
    )
    where
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder.ByteString as BB
#if defined(MIN_VERSION_QuickCheck)
import Control.Applicative
import Control.Applicative.Unicode
#endif
import Control.DeepSeq
import Control.Failure
import Data.Attoparsec.Char8
import Data.CaseInsensitive as CI
import Data.Hashable
import Data.Monoid.Unicode
import Data.String
import Data.Semigroup
import Data.Typeable
import Data.URI.Internal () -- for orphan instances for ByteString
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import qualified Data.Vector.Storable.ByteString.Char8 as C8
import Data.Vector.Storable.ByteString.Legacy
import Prelude hiding (takeWhile)
import Prelude.Unicode
#if defined(MIN_VERSION_QuickCheck)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
#endif

-- |'Scheme' names consist of a non-empty sequence of characters
-- beginning with a letter and followed by any combination of letters,
-- digits, plus (\'+\'), period (\'.\'), or hyphen (\'-\'). Comparison
-- of 'Scheme's are always case-insensitive. See:
-- <http://tools.ietf.org/html/rfc3986#section-3.1>
newtype Scheme = Scheme { unScheme ∷ CI ByteString }
    deriving ( Eq
             , FoldCase
             , Hashable
             , NFData
             , Ord
             , Typeable
             )

-- |For testing purpose only.
instance Show Scheme where
    show = C8.unpack ∘ foldedCase ∘ unScheme

-- |'fromString' is a fast but unsafe way to create 'Scheme' such that
-- no validation on the string is performed.
deriving instance IsString Scheme

-- |'Scheme's form a 'Semigroup' with string concatenation as the
-- operation. Since 'Scheme's can not be empty, they don't form a
-- 'Monoid'.
instance Semigroup Scheme where
    {-# INLINE (<>) #-}
    Scheme a <> Scheme b = Scheme (a ⊕ b)

-- |'Parser' for 'Scheme's which fails without consuming any input if
-- the first letter is not an ASCII alphabet.
parser ∷ Parser Scheme
{-# INLINEABLE parser #-}
parser = do x  ← satisfy first
            xs ← fromLegacyByteString <$> takeWhile nonFirst
            pure ∘ Scheme ∘ CI.mk $ x `C8.cons` xs
         <?>
         "scheme"
    where
      {-# INLINE first #-}
      first = isAlpha_ascii
      {-# INLINE nonFirst #-}
      nonFirst c
          = isAlpha_ascii c ∨
            isDigit c       ∨
            c ≡ '+'         ∨
            c ≡ '-'         ∨
            c ≡ '.'

-- |Create a 'Builder' from a 'Scheme' with all letters lowercased.
toBuilder ∷ Scheme → Builder
{-# INLINE toBuilder #-}
toBuilder = BB.fromByteString  ∘
            toLegacyByteString ∘
            foldedCase         ∘
            unScheme

-- |Try to parse a 'Scheme' from an ascii string.
fromByteString ∷ Failure String f ⇒ ByteString → f Scheme
{-# INLINE fromByteString #-}
fromByteString = either failure return ∘
                 parseOnly parser      ∘
                 toLegacyByteString

#if defined(MIN_VERSION_QuickCheck)
instance Arbitrary Scheme where
    arbitrary = (fromString ∘) ∘ (:) <$> x ⊛ xs
        where
          genAlpha = elements (['a'..'z'] ⊕ ['A'..'Z'])
          genDigit = elements  ['0'..'9']
          genSym   = elements  ['+', '-', '.']
          x        = genAlpha
          xs       = listOf $ oneof [genAlpha, genDigit, genSym]

    shrink = (fromString <$>) ∘ shr ∘ show ∘ unScheme
        where
          -- Make sure not to produce empty strings.
          shr ∷ [Char] → [String]
          shr []       = error "internal error"
          shr (_:[])   = []
          shr (x:y:ys) = (x:ys) : ((x:) <$> shr (y:ys))

instance CoArbitrary Scheme where
    coarbitrary = coarbitrary ∘ show ∘ unScheme
#endif
