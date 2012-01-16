{-# LANGUAGE
    UnicodeSyntax
  #-}
module Data.URI.Internal
    ( isUnreserved
    , isPctEncoded
    , isHexDigit_w8
    , isSubDelim

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
import qualified Data.Attoparsec as B
import Data.Attoparsec.Char8
import Data.ByteString (ByteString)
import Data.Word
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
