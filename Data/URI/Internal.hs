{-# LANGUAGE
    ScopedTypeVariables
  , TypeSynonymInstances
  , UnicodeSyntax
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.URI.Internal
    ( isUnreserved
    , isPctEncoded
    , isHexDigit_w8
    , isSubDelim

    , finishOff
    )
    where
import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Unicode
import qualified Data.Attoparsec as B
import Data.Attoparsec.Char8
import Data.CaseInsensitive
import Data.Char
import Data.Hashable
import Data.Vector.Fusion.Util
import qualified Data.Vector.Storable as SV
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import qualified Data.Vector.Storable.ByteString.Char8 as C8
import Data.Word
import Foreign.ForeignPtr
import Foreign.Storable
import Prelude.Unicode

isUnreserved ∷ Char → Bool
{-# INLINE isUnreserved #-}
isUnreserved = inClass "a-zA-Z0-9._~-"

isPctEncoded ∷ Char → Bool
{-# INLINE isPctEncoded #-}
isPctEncoded = inClass "%a-fA-F0-9"

isHexDigit_w8 ∷ Word8 → Bool
{-# INLINE isHexDigit_w8 #-}
isHexDigit_w8 = B.inClass "a-fA-F0-9"

isSubDelim ∷ Char → Bool
{-# INLINE isSubDelim #-}
isSubDelim = inClass "!$&'()⋅+,;="

finishOff ∷ Parser α → Parser α
{-# INLINE finishOff #-}
finishOff = ((endOfInput *>) ∘ return =≪)

-- FIXME: Remove this when the vector starts providing Hashable
-- instances.
instance (Hashable α, Storable α) ⇒ Hashable (SV.Vector α) where
    {-# INLINE hashWithSalt #-}
    hashWithSalt salt sv = unsafeInlineIO $
                           withForeignPtr fp $ \p →
                           hashPtrWithSalt p (fromIntegral len) salt
        where
          (fp, n) = SV.unsafeToForeignPtr0 sv
          len     = n ⋅ sizeOf ((⊥) ∷ α)

-- FIXME: Remove this when the vector-bytestring starts providing
-- FoldCase instances.
instance FoldCase ByteString where
    {-# INLINE foldCase #-}
    foldCase = C8.map toLower

-- FIXME: Remove this when the Id starts providing Applicative
-- instances.
instance Applicative Id where
    {-# INLINE pure #-}
    pure = return
    {-# INLINE (<*>) #-}
    (<*>) = ap
