{-# LANGUAGE
    ScopedTypeVariables
  , UnicodeSyntax
  #-}
module Data.URI.Internal
    ( isAlpha_w8
    , isHexDigit_w8
    , isUnreserved_w8

    , PctState(..)
    , scanPctEncoded
    , pctDecode

    , finishOff
    , parseAttempt
    , parseAttempt'
    )
    where
import Control.Applicative
import Control.Applicative.Unicode ((∅))
import Control.Exception.Base
import Control.Monad.Unicode
import Data.Ascii (Ascii)
import qualified Data.Ascii as A
import Data.Attempt
import Data.Attoparsec.Char8 hiding (Done)
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import Data.Default
import Data.Monoid.Unicode ((⊕))
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Prelude.Unicode
import System.IO.Unsafe

isAlpha_w8 ∷ Word8 → Bool
{-# INLINE isAlpha_w8 #-}
isAlpha_w8 w = (w ≥ 0x41 ∧ w ≤ 0x5A) ∨ -- 'A' ≤ w ≤ 'Z'
               (w ≥ 0x61 ∧ w ≤ 0x7A)   -- 'a' ≤ w ≤ 'z'

isHexDigit_w8 ∷ Word8 → Bool
{-# INLINE isHexDigit_w8 #-}
isHexDigit_w8 w = (w ≥ 0x30 ∧ w ≤ 0x39) ∨ -- '0' ≤ w ≤ '9'
                  (w ≥ 0x41 ∧ w ≤ 0x46) ∨ -- 'A' ≤ w ≤ 'F'
                  (w ≥ 0x61 ∧ w ≤ 0x66)   -- 'a' ≤ w ≤ 'f'

isUnreserved_w8 ∷ Word8 → Bool
{-# INLINE isUnreserved_w8 #-}
isUnreserved_w8 w = isAlpha_w8 w ∨
                    isDigit_w8 w ∨
                    w ≡ 0x2D     ∨ -- '-'
                    w ≡ 0x2E     ∨ -- '.'
                    w ≡ 0x5F     ∨ -- '_'
                    w ≡ 0x7E       -- '~'

data PctState = Initial
              | Percent
              | UpperHalf !Word8 -- ^upperhalf letter
              | Failed    !Word8 -- ^erroneous letter
              | Done       Word8 -- ^resulting octet
                deriving Show

instance Default PctState where
    {-# INLINE CONLIKE def #-}
    def = Initial

scanPctEncoded ∷ PctState → Word8 → PctState
{-# INLINEABLE scanPctEncoded #-}
scanPctEncoded Initial w
    | w ≡ 0x25        = Percent
    | otherwise       = Initial
scanPctEncoded Percent u
    | isHexDigit_w8 u = UpperHalf u
    | otherwise       = Failed u
scanPctEncoded (UpperHalf u) l
    | isHexDigit_w8 l = Done $ unsafeHexDecode u l
    | otherwise       = Failed l
scanPctEncoded s _
    = error $ "scanPctEncoded: internal error: " ⊕ show s

unsafeHexDecode ∷ Word8 → Word8 → Word8
{-# INLINEABLE unsafeHexDecode #-}
unsafeHexDecode u l = (decode u `shiftL` 4) .|. decode l
    where
      decode ∷ Word8 → Word8
      {-# INLINEABLE decode #-}
      decode w
          | w ≥ 0x30 ∧ w ≤ 0x39 = w - 0x30      -- '0'..'9'
          | w ≥ 0x61            = w - 0x61 + 10 -- 'a'..'f'
          | otherwise           = w - 0x65 + 10 -- 'A'..'F'

-- |Fast, but VERY unsafe implementation of percent-decoder.
pctDecode ∷ ∀f. Alternative f ⇒ ByteString → f ByteString
{-# INLINEABLE pctDecode #-}
pctDecode src = unsafePerformIO decode
    where
      decode ∷ IO (f ByteString)
      {-# INLINEABLE decode #-}
      decode = do let (fpIn, srcOff, srcLen) = BS.toForeignPtr src
                  fpOut ← BS.mallocByteString srcLen
                  withForeignPtr fpIn $ \pIn →
                    withForeignPtr fpOut $ \pOut →
                    do nWrote ← go Initial srcLen 0 (pIn `plusPtr` srcOff) 0 pOut
                       case nWrote of
                         Just n  → pure ∘ pure $ BS.fromForeignPtr fpOut 0 n
                         Nothing → pure (∅) -- failed
      go ∷ PctState
         → Int
         → Int
         → Ptr Word8
         → Int
         → Ptr Word8
         → IO (Maybe Int)
      {-# INLINEABLE go #-}
      go s srcLen nRead pIn nWrote pOut
          | nRead ≥ srcLen
              = case s of
                  Initial → pure (Just nWrote)
                  _       → pure Nothing -- premature EOF
          | otherwise
              = do w ← peek pIn
                   case scanPctEncoded s w of
                     s'@Initial
                         → poke pOut w *>
                           go s' srcLen (nRead + 1) (pIn `plusPtr` 1) (nWrote + 1) (pOut `plusPtr` 1)
                     s'@Percent
                         → go s' srcLen (nRead + 1) (pIn `plusPtr` 1) nWrote pOut
                     s'@(UpperHalf _)
                         → go s' srcLen (nRead + 1) (pIn `plusPtr` 1) nWrote pOut
                     Failed _
                         → pure Nothing
                     Done w'
                         → poke pOut w' *>
                           go Initial srcLen (nRead + 1) (pIn `plusPtr` 1) (nWrote + 1) (pOut `plusPtr` 1)

finishOff ∷ Parser α → Parser α
{-# INLINE finishOff #-}
finishOff = ((endOfInput *>) ∘ return =≪)

parseAttempt ∷ Exception e
             ⇒ (String → e)
             → Parser α
             → ByteString
             → Attempt α
{-# INLINEABLE parseAttempt #-}
parseAttempt f p bs
    = case parseOnly (finishOff p) bs of
        Right α → Success α
        Left  e → Failure $ f e

parseAttempt' ∷ Parser α → Ascii → Attempt α
{-# INLINE parseAttempt' #-}
parseAttempt' = (∘ A.toByteString) ∘ parseAttempt StringException
