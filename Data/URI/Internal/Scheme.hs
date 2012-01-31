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
module Data.URI.Internal.Scheme
    ( Scheme
    )
    where
#if defined(MIN_VERSION_QuickCheck)
import Control.Applicative
import Control.Applicative.Unicode
#endif
import Data.Ascii (AsciiBuilder, CIAscii)
import qualified Data.Ascii as A
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive
import Data.Convertible.Base
import Data.Convertible.Instances.Ascii ()
import Data.Convertible.Utils
import Data.Default
import Data.Hashable
import Data.Monoid.Unicode
import Data.String
import Data.Semigroup
import Data.URI.Internal
import Data.Typeable
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
newtype Scheme = Scheme { unScheme ∷ CIAscii }
    deriving ( Eq
             , FoldCase
             , Hashable
             , Ord
             , Show
             , Typeable
             )

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
instance Default (Parser Scheme) where
    {-# INLINEABLE def #-}
    def = do x  ← satisfy first
             xs ← takeWhile nonFirst
             return ∘ fromBS $ x `BS.cons` xs
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
          {-# INLINE fromBS #-}
          fromBS = Scheme ∘ cs ∘ A.unsafeFromByteString

-- |Extract a 'CIAscii' from 'Scheme' with all letters lowercased.
instance ConvertSuccess Scheme CIAscii where
    {-# INLINE convertSuccess #-}
    convertSuccess = foldCase ∘ unScheme

-- |Create an 'AsciiBuilder' from 'Scheme' with all letters
-- lowercased.
instance ConvertSuccess Scheme AsciiBuilder where
    {-# INLINE convertSuccess #-}
    convertSuccess = convertSuccessVia ((⊥) ∷ CIAscii)

-- |Try to parse a 'Scheme' from 'CIAscii'.
instance ConvertAttempt CIAscii Scheme where
    {-# INLINE convertAttempt #-}
    convertAttempt = parseAttempt' def ∘ cs

deriveAttempts [ ([t| Scheme |], [t| AsciiBuilder |])
               , ([t| Scheme |], [t| CIAscii      |])
               ]

#if defined(MIN_VERSION_QuickCheck)
instance Arbitrary Scheme where
    arbitrary = (fromString ∘) ∘ (:) <$> x ⊛ xs
        where
          genAlpha = elements (['a'..'z'] ⊕ ['A'..'Z'])
          genDigit = elements  ['0'..'9']
          genSym   = elements  ['+', '-', '.']
          x        = genAlpha
          xs       = listOf $ oneof [genAlpha, genDigit, genSym]

    shrink = (fromString <$>) ∘ shr ∘ cs ∘ unScheme
        where
          shr ∷ [Char] → [String]
          shr []       = error "internal error"
          shr (_:[])   = []
          shr (x:y:ys) = (x:ys) : ((x:) <$> shr (y:ys))

instance CoArbitrary Scheme where
    coarbitrary = coarbitrary ∘ toString ∘ unScheme
        where
          toString ∷ CIAscii → String
          toString = cs
#endif
