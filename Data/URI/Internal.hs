{-# LANGUAGE
    UnicodeSyntax
  #-}
module Data.URI.Internal
    ( finishOff
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
import Prelude.Unicode

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
