{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , UnicodeSyntax
  , ViewPatterns
  #-}
module Data.URI.Internal.Scheme
    ( Scheme(..)
    , parser
    , fromByteString
    , toBuilder
    )
    where
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder.ByteString as BB
import Control.Applicative
#if defined(MIN_VERSION_QuickCheck)
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

-- |'Scheme's form a 'Semigroup' with string concatenation as the
-- operation. Since 'Scheme's can not be empty, they don't form a
-- 'Monoid'.
instance Semigroup Scheme where
    {-# INLINE (<>) #-}
    Scheme a <> Scheme b = Scheme (a ⊕ b)

-- |'fromString' constructs a 'Scheme' from a 'String'. Throws a
-- runtime exception for invalid schemes.
instance IsString Scheme where
    {-# INLINEABLE fromString #-}
    fromString (toLegacyByteString ∘ C8.pack → str)
        = case parseOnly parser str of
            Right s → s
            Left  e → error e

-- |'Parser' for 'Scheme's.
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
