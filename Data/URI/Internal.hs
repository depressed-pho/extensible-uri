{-# LANGUAGE
    FlexibleInstances
  , ScopedTypeVariables
  , UnicodeSyntax
  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.URI.Internal
    ( isUnreserved
    , isPctEncoded
    , isHexDigit_w8
    , isSubDelim
    , inRange_w8

    , atoi
    , htoi

    , countV
    , countUpToV
    , countUpTo
    , countUpTo1
    , finishOff
    )
    where
import Control.Applicative
import Control.Applicative.Unicode
import Control.DeepSeq
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Unicode
import qualified Data.Attoparsec as B
import Data.Attoparsec.Char8
import Data.Vector.Storable.ByteString.Internal (c2w)
import Data.CaseInsensitive
import Data.Char
import Data.Hashable
import Data.Vector.Fusion.Util
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Storable as SV
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import qualified Data.Vector.Storable.ByteString.Char8 as C8
import qualified Data.Vector.Unboxed as UV
import Data.Word
import Foreign.ForeignPtr
import Foreign.Storable
import Numeric.Natural
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

inRange_w8 ∷ Char → Char → Word8 → Bool
{-# INLINE inRange_w8 #-}
inRange_w8 x y w
    = c2w x ≤ w ∧ w ≤ c2w y

atoi ∷ Integral n ⇒ Word8 → n
{-# INLINE atoi #-}
atoi = subtract 0x30 ∘ fromIntegral

htoi ∷ Integral n ⇒ Word8 → n
{-# INLINEABLE htoi #-}
htoi w | w ≥ 0x30 ∨ w ≤ 0x39 = fromIntegral (w - 0x30)
       | w ≥ 0x61            = fromIntegral (w - 0x57)
       | otherwise           = fromIntegral (w - 0x37)

countV ∷ (GV.Vector v α, Functor m, Monad m) ⇒ Int → m α → m (v α)
{-# INLINE countV #-}
countV = ((GV.fromList <$>) ∘) ∘ count

countUpToV ∷ (GV.Vector v α, Alternative f) ⇒ Int → f α → f (v α)
{-# INLINE countUpToV #-}
countUpToV = ((GV.fromList <$>) ∘) ∘ countUpTo

countUpTo ∷ Alternative f ⇒ Int → f α → f [α]
{-# INLINEABLE countUpTo #-}
countUpTo 0 _ = pure []
countUpTo n p = ((:) <$> p ⊛ countUpTo (n-1) p) <|> pure []

countUpTo1 ∷ Alternative f ⇒ Int → f α → f [α]
{-# INLINE countUpTo1 #-}
countUpTo1 n p = (:) <$> p ⊛ countUpTo (n-1) p

finishOff ∷ Parser α → Parser α
{-# INLINE finishOff #-}
finishOff = ((endOfInput *>) ∘ pure =≪)

-- FIXME: Remove this when the vector starts providing Hashable
-- instances.
instance (Hashable α, Storable α) ⇒ Hashable (SV.Vector α) where
    {-# INLINEABLE hashWithSalt #-}
    hashWithSalt salt sv = unsafeInlineIO $
                           withForeignPtr fp $ \p →
                           hashPtrWithSalt p (fromIntegral len) salt
        where
          (fp, n) = SV.unsafeToForeignPtr0 sv
          len     = n ⋅ sizeOf ((⊥) ∷ α)

-- FIXME: Remove this when the vector starts providing Hashable
-- instances. Unboxed vectors don't expose their internal
-- representation (ByteArray#) so we can't implement an efficient
-- instance.
instance (Hashable α, UV.Unbox α) ⇒ Hashable (UV.Vector α) where
    {-# INLINE hashWithSalt #-}
    hashWithSalt = UV.foldl' hashWithSalt

-- FIXME: Remove this when the nats starts providing Hashable instance.
instance Hashable Natural where
    {-# INLINE hashWithSalt #-}
    hashWithSalt salt n
        = salt `hashWithSalt` toInteger n

-- FIXME: Remove this when the nats starts providing NFData instance.
instance NFData Natural

-- FIXME: Remove this when the vector-bytestring starts providing
-- FoldCase instances.
instance FoldCase ByteString where
    {-# INLINE foldCase #-}
    foldCase = C8.map toLower

-- FIXME: Remove this when the Id starts providing Applicative
-- instance.
instance Applicative Id where
    {-# INLINE pure #-}
    pure = return
    {-# INLINE (<*>) #-}
    (<*>) = ap
