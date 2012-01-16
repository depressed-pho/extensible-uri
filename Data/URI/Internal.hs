{-# LANGUAGE
    UnicodeSyntax
  #-}
module Data.URI.Internal
    ( isAlpha_w8
    , isHexDigit_w8
    , isUnreserved_w8

    , finishOff
    , parseAttempt
    , parseAttempt'
    )
    where
import Control.Applicative
import Control.Exception.Base
import Control.Monad.Unicode
import Data.Ascii (Ascii)
import qualified Data.Ascii as A
import Data.Attempt
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import Data.Word
import Prelude.Unicode

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
