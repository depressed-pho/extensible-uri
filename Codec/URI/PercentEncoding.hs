{-# LANGUAGE
    FlexibleContexts
  , ScopedTypeVariables
  , UnicodeSyntax
  #-}
-- |Fast percent-encoding and decoding for ByteStrings.
module Codec.URI.PercentEncoding
    ( encode
    , decode

    , DecodingFailed
    )
    where
import Control.Applicative
import Control.Monad.Failure.Transformers
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Error
import Data.Ascii (Ascii)
import qualified Data.Ascii as A
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Internal as BS
import Data.Monoid.Unicode
import Data.Vector.Fusion.Stream.Monadic
import Data.Vector.Fusion.Stream.Size
import Data.URI.Internal
import Data.Word
import Foreign.ForeignPtr (ForeignPtr)
import qualified Foreign.ForeignPtr as FP
import Foreign.Ptr
import Foreign.Storable (Storable)
import qualified Foreign.Storable as S
import System.IO.Unsafe
import Prelude.Unicode

data EncState = EInitial
              | EPercent   !Word8 !Word8 -- upper and lower halves
              | EUpperHalf !Word8        -- lower half

data DecState = DInitial
              | DPercent
              | DUpperHalf !Word8        -- upper half

-- |Data type to represent a failure of decoding percent-encoded
-- strings.
data DecodingFailed = DecodingFailed !(Maybe String)

instance Error DecodingFailed where
    {-# INLINE CONLIKE noMsg #-}
    noMsg = DecodingFailed Nothing
    {-# INLINE CONLIKE strMsg #-}
    strMsg = DecodingFailed ∘ Just

instance Show DecodingFailed where
    show (DecodingFailed Nothing)
        = "DecodingFailed"
    show (DecodingFailed (Just msg))
        = "DecodingFailed: " ⊕ msg


-- |Decode a percent-encoded 'Ascii' string to a 'ByteString'.
decode ∷ ∀f. (Applicative f, Failure DecodingFailed f) ⇒ Ascii → f ByteString
{-# INLINE decode #-}
decode = decodeInIO ∘ A.toByteString
    where
      decodeInIO ∷ ByteString → f ByteString
      {-# INLINE decodeInIO #-}
      decodeInIO src
          = unsafePerformIO $
            do r ← runErrorT $ decodeInErrorT src
               case r of
                 Right dst → pure $ pure dst
                 Left  e   → pure $ failure e

      decodeInErrorT ∷ ByteString → ErrorT DecodingFailed IO ByteString
      {-# INLINE decodeInErrorT #-}
      decodeInErrorT = unstreamBS ∘ decodeStream ∘ streamBS

-- |Percent-encode a 'ByteString' to 'Ascii' using a predicate to
-- determine which letters are to be encoded.
encode ∷ (Char → Bool) → ByteString → Ascii
{-# INLINE encode #-}
encode f = A.unsafeFromByteString ∘ encodeInIO
    where
      encodeInIO ∷ ByteString → ByteString
      {-# INLINE encodeInIO #-}
      encodeInIO
          = unsafePerformIO ∘ unstreamBS ∘ encodeStream g ∘ streamBS

      g ∷ Word8 → Bool
      g = f ∘ A.toChar

streamBS ∷ ∀f. Applicative f ⇒ ByteString → Stream f Word8
{-# INLINE streamBS #-}
streamBS bs = Stream go 0 (Exact len)
    where
      len ∷ Int
      {-# INLINE CONLIKE len #-}
      len = BS.length bs

      go ∷ Int → f (Step Int Word8)
      {-# INLINE go #-}
      go n | n ≥ len   = pure Done
           | otherwise = pure $ Yield (BS.unsafeIndex bs n) (n + 1)

unstreamBS ∷ ∀m. MonadBaseControl IO m ⇒ Stream m Word8 → m ByteString
{-# INLINE unstreamBS #-}
unstreamBS (Stream step (s0 ∷ s) sz)
    = case upperBound sz of
        Just n  → unstreamWithMaxLen n
        Nothing → fail "unstreamBS: stream with an unknown size is not currently supported"
    where
      unstreamWithMaxLen ∷ Int → m ByteString
      {-# INLINE unstreamWithMaxLen #-}
      unstreamWithMaxLen n
          = do fpOut ← mallocByteString n
               withForeignPtr fpOut $ \pOut →
                 do nWrote ← go 0 pOut s0
                    let out = BS.fromForeignPtr fpOut 0 nWrote
                    if nWrote ≡ n then
                        pure out
                    else
                        pure $ BS.copy out

      go ∷ Int → Ptr Word8 → s → m Int
      {-# INLINE go #-}
      go nWrote pOut s
          = do r ← step s
               case r of
                 Yield w s' → poke pOut w *>
                              go (nWrote + 1) (pOut `plusPtr` 1) s'
                 Skip    s' → go nWrote pOut s'
                 Done       → pure nWrote

encodeStream ∷ ∀f. ( Applicative f
                   , Monad f
                   )
             ⇒ (Word8 → Bool)
             → Stream f Word8
             → Stream f Word8
{-# INLINE encodeStream #-}
encodeStream isUnsafe (Stream step (s0 ∷ s) sz)
    = case upperBound sz of
        Just n  → Stream (uncurry go) (EInitial, s0) (Max $ n ⋅ 3)
        Nothing → error "encodeStream: stream with an unknown size is not currently supported"
    where
      go ∷ EncState → s → f (Step (EncState, s) Word8)
      {-# INLINE go #-}
      go EInitial s
          = do r ← step s
               case r of
                 Yield w s'
                     | isUnsafe w → let (u, l) = encodeHex w
                                    in pure $ Yield 0x25 (EPercent u l, s')
                     | otherwise  → pure $ Yield w (EInitial, s')
                 Skip    s'       → pure $ Skip    (EInitial, s')
                 Done             → pure $ Done
      go (EPercent u l) s         = pure $ Yield u (EUpperHalf l, s)
      go (EUpperHalf l) s         = pure $ Yield l (EInitial    , s)

decodeStream ∷ ∀f. ( Applicative f
                   , Failure DecodingFailed f
                   )
             ⇒ Stream f Word8
             → Stream f Word8
{-# INLINE decodeStream #-}
decodeStream (Stream step (s0 ∷ s) sz) = Stream (uncurry go) (DInitial, s0) $ toMax sz
    where
      go ∷ DecState → s → f (Step (DecState, s) Word8)
      {-# INLINE go #-}
      go ds s
          = do r ← step s
               case r of
                 Yield w s' → gotOctet ds s' w
                 Skip    s' → pure $ Skip (ds, s')
                 Done       → gotEOF ds

      gotOctet ∷ DecState → s → Word8 → f (Step (DecState, s) Word8)
      {-# INLINE gotOctet #-}
      gotOctet DInitial s 0x25 = pure $ Skip    (DPercent, s) -- '%'
      gotOctet DInitial s w    = pure $ Yield w (DInitial, s) -- unencoded
      gotOctet DPercent s u
          | isHexDigit_w8 u    = pure $ Skip (DUpperHalf u, s)
          | otherwise          = let e ∷ DecodingFailed
                                     e = strMsg "non-hex-digit occured at the upper half of %xx"
                                 in failure e
      gotOctet (DUpperHalf u) s l
          | isHexDigit_w8 l    = let w = unsafeDecodeHex u l
                                 in pure $ Yield w (DInitial, s)
          | otherwise          = let e ∷ DecodingFailed
                                     e = strMsg "non-hex-digit occured at the lower half of %xx"
                                 in failure e

      gotEOF ∷ DecState → f (Step σ Word8)
      {-# INLINE gotEOF #-}
      gotEOF DInitial = pure Done
      gotEOF _        = let e ∷ DecodingFailed
                            e = strMsg "premature end of percent-encoded string"
                        in failure e

encodeHex ∷ Word8 → (Word8, Word8)
{-# INLINEABLE encodeHex #-}
encodeHex w = (encodeHalf $ w `shiftR` 4, encodeHalf $ w .&. 0x0F)
    where
      encodeHalf ∷ Word8 → Word8
      {-# INLINEABLE encodeHalf #-}
      encodeHalf h
          | h < 0x0A  = h      + 0x30 -- '0'..'9'
          | otherwise = h - 10 + 0x65 -- 'A'..'F'

unsafeDecodeHex ∷ Word8 → Word8 → Word8
{-# INLINEABLE unsafeDecodeHex #-}
unsafeDecodeHex u l = (decodeHalf u `shiftL` 4) .|. decodeHalf l
    where
      decodeHalf ∷ Word8 → Word8
      {-# INLINEABLE decodeHalf #-}
      decodeHalf w
          | w ≥ 0x30 ∧ w ≤ 0x39 = w - 0x30      -- '0'..'9'
          | w ≥ 0x61            = w - 0x61 + 10 -- 'a'..'f'
          | otherwise           = w - 0x65 + 10 -- 'A'..'F'

mallocByteString ∷ MonadBase IO m ⇒ Int → m (ForeignPtr Word8)
{-# INLINE mallocByteString #-}
mallocByteString = liftBase ∘ BS.mallocByteString

withForeignPtr ∷ MonadBaseControl IO m ⇒ ForeignPtr α → (Ptr α → m β) → m β
{-# INLINE withForeignPtr #-}
withForeignPtr fp f
    = control $ \runInIO →
        FP.withForeignPtr fp $ \p →
          runInIO $ f p

poke ∷ (MonadBaseControl IO m, Storable α) ⇒ Ptr α → α → m ()
{-# INLINE poke #-}
poke = (liftBase ∘) ∘ S.poke
