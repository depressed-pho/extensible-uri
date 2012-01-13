{-# LANGUAGE
    DeriveDataTypeable
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
import Data.Ascii (AsciiBuilder, CIAscii)
import qualified Data.Ascii as A
import Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive
import Data.Convertible.Base
import Data.Default
import Data.Hashable
import Data.Monoid.Unicode
import Data.String
import Data.Semigroup
import Data.URI.Internal
import Data.Typeable
import Prelude hiding (takeWhile)
import Prelude.Unicode

-- |'Scheme' names consist of a non-empty sequence of characters
-- beginning with a letter and followed by any combination of letters,
-- digits, plus (\'+\'), period (\'.\'), or hyphen (\'-\'). Comparison
-- of 'Scheme's are always case-insensitive. See:
-- <http://tools.ietf.org/html/rfc3986#section-3.1>
newtype Scheme = Scheme CIAscii
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

-- |'Scheme' forms a 'Semigroup' with string concatenation as the
-- operation. Since 'Scheme's can not be empty, they don't form a
-- 'Monoid'.
instance Semigroup Scheme where
    {-# INLINE (<>) #-}
    Scheme a <> Scheme b = Scheme (a ⊕ b)

-- |'Parser' for 'Scheme's. May fail if the first letter is not an
-- ASCII alphabet.
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
          fromBS = Scheme ∘ A.toCIAscii ∘ A.unsafeFromByteString

-- |Extract a 'CIAscii' with all letters lowercased.
instance ConvertSuccess Scheme CIAscii where
    {-# INLINE convertSuccess #-}
    convertSuccess (Scheme s) = foldCase s

-- |Create an 'AsciiBuilder' with all letters lowercased.
instance ConvertSuccess Scheme AsciiBuilder where
    {-# INLINE convertSuccess #-}
    convertSuccess = A.toAsciiBuilder ∘ A.fromCIAscii ∘ cs

-- |Try to parse a 'Scheme' from 'CIAscii'.
instance ConvertAttempt CIAscii Scheme where
    {-# INLINE convertAttempt #-}
    convertAttempt = parseAttempt' def ∘ A.fromCIAscii

deriveAttempts [ ([t| Scheme |], [t| AsciiBuilder |])
               , ([t| Scheme |], [t| CIAscii      |])
               ]
