{-# LANGUAGE
    CPP
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeFamilies
  , UnicodeSyntax
  , ViewPatterns
  #-}
-- |Fast percent-encoding and decoding for ByteStrings.
module Codec.URI.PercentEncoding
    ( DelimitableOctet(..)
    , DelimitedByteString
    , DecodeError(..)
    , encode
    , decode
    )
    where
import Control.Applicative
import Control.DeepSeq
import Control.Exception.Base
import Control.Failure
import Data.Bits
import Data.ByteString.Internal (w2c)
import Data.Hashable
import Data.Monoid.Unicode
import Data.String
import Data.Typeable
import Data.Vector.Storable.ByteString (ByteString)
import qualified Data.Vector.Storable.ByteString.Char8 as C8
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Fusion.Stream as PS
import Data.Vector.Fusion.Stream.Monadic
import Data.Vector.Fusion.Stream.Size
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.URI.Internal
import Data.Word
import Prelude.Unicode
#if defined(MIN_VERSION_QuickCheck)
import Test.QuickCheck.Arbitrary
#endif

-- |FIXME: docs
data DelimitableOctet
    = Delimiter !Word8
    | Literal   !Word8
    deriving (Eq, Ord)

instance Hashable DelimitableOctet where
    hashWithSalt salt (marshal → (isDelim, w))
        = salt `hashWithSalt` isDelim `hashWithSalt` w

instance NFData DelimitableOctet

#if defined(MIN_VERSION_QuickCheck)
instance Arbitrary DelimitableOctet where
    arbitrary =  unmarshal <$> arbitrary
    shrink    = (unmarshal <$>) ∘ shrink ∘ marshal

instance CoArbitrary DelimitableOctet where
    coarbitrary = coarbitrary ∘ marshal
#endif

marshal ∷ DelimitableOctet → (Bool, Word8)
{-# INLINE marshal #-}
marshal (Delimiter w) = (True , w)
marshal (Literal   w) = (False, w)

unmarshal ∷ (Bool, Word8) → DelimitableOctet
{-# INLINE unmarshal #-}
unmarshal (True , w) = Delimiter w
unmarshal (False, w) = Literal   w

derivingUnbox "DelimitableOctet"
    [t| DelimitableOctet → (Bool, Word8) |]
    [| marshal   |]
    [| unmarshal |]

-- |Decode every percent-encoded octets and turn every letters to
-- 'Literal's. Throws a runtime exception for 'DecodeError's.
instance IsString (UV.Vector DelimitableOctet) where
    {-# INLINEABLE fromString #-}
    fromString str
        = case decode (const False) (C8.pack str) of
            Right v → v
            Left  e → throw (e ∷ DecodeError)

instance Hashable (UV.Vector DelimitableOctet) where
    {-# INLINE hashWithSalt #-}
    hashWithSalt = UV.foldl' hashWithSalt

#if defined(MIN_VERSION_QuickCheck)
instance Arbitrary (UV.Vector DelimitableOctet) where
    arbitrary =  UV.fromList <$> arbitrary
    shrink    = (UV.fromList <$>) ∘ shrink ∘ UV.toList

instance CoArbitrary (UV.Vector DelimitableOctet) where
    coarbitrary = coarbitrary ∘ UV.toList
#endif

-- |FIXME: doc
type DelimitedByteString
    = UV.Vector DelimitableOctet

data EncState s
    = EInitial   !s
    | EPercent   !s !Word8 !Word8 -- ^upper and lower halves
    | EUpperHalf !s !Word8        -- ^lower half

data DecState s
    = DInitial   !s
    | DPercent   !s
    | DUpperHalf !s !Word8 -- ^upper half

-- |Data type to represent a decoding error of percent-encoded
-- strings.
data DecodeError
    = InvalidUpperHalf !Char       -- ^invalid upper half
    | InvalidLowerHalf !Char !Char -- ^valid upper and invalid lower halves
    | MissingUpperHalf
    | MissingLowerHalf !Char       -- ^valid upper half
    deriving Typeable

instance Exception DecodeError

instance Show DecodeError where
    show (InvalidUpperHalf u)
        = "DecodeError: non-hex-digit occured after \"%\": '" ⊕ [u] ⊕ "'"
    show (InvalidLowerHalf u l)
        = "DecodeError: non-hex-digit occured after \"%" ⊕ [u] ⊕ "\": '" ⊕ [l] ⊕ "'"
    show MissingUpperHalf
        = "DecodeError: premature end with \"%\""
    show (MissingLowerHalf u)
        = "DecodeError: premature end with \"%" ⊕ [u] ⊕ "\""

-- |Encode a 'DelimitedByteString' to percent-encoded ascii string
-- using a predicate to determine which 'Literal's should be
-- encoded. Note that 'Delimiter's are always passed through.
encode ∷ (Char → Bool) → DelimitedByteString → ByteString
{-# INLINE encode #-}
encode isUnsafe = GV.unstream ∘ encodeStream isUnsafe ∘ GV.stream

-- |Decode a percent-encoded ascii string to 'DelimitedByteString'
-- using a predicate to determine which non-encoded letters should be
-- considered to be delimiters. Note that encoded octets are always
-- considered to be 'Literal'.
decode ∷ ∀f. (Applicative f, Failure DecodeError f)
       ⇒ (Char → Bool)
       → ByteString
       → f DelimitedByteString
{-# INLINE decode #-}
decode isDelim = munstream ∘ decodeStream isDelim ∘ mstream

mstream ∷ (Monad m, GV.Vector v α) ⇒ v α → Stream m α
{-# INLINE mstream #-}
mstream = PS.liftStream ∘ GV.stream

-- THINKME: This is terrible, but what else can we do? See:
-- http://trac.haskell.org/vector/ticket/81
munstream ∷ (Functor m, Monad m, GV.Vector v α) ⇒ Stream m α → m (v α)
{-# INLINE munstream #-}
munstream s = GV.unstream ∘ PS.unsafeFromList (size s) <$> toList s

encodeStream ∷ ∀f. (Applicative f, Monad f)
             ⇒ (Char → Bool)
             → Stream f DelimitableOctet
             → Stream f Word8
{-# INLINE encodeStream #-}
encodeStream isUnsafe (Stream step (s0 ∷ s) sz)
    = Stream go (EInitial s0) sz'
    where
      sz' ∷ Size
      {-# INLINE sz' #-}
      sz' = case upperBound sz of
              Just n  → Max $ n ⋅ 3
              Nothing → Unknown

      go ∷ EncState s → f (Step (EncState s) Word8)
      {-# INLINE go #-}
      go (EInitial s)
          = do r ← step s
               case r of
                 Yield (Delimiter w) s'
                          → pure $ Yield w    (EInitial s'    )
                 Yield (Literal   w) s'
                     | isUnsafe (w2c w)
                          → let (u, l) = encodeHex w in
                            pure $ Yield 0x25 (EPercent s' u l)
                     | otherwise
                          → pure $ Yield w    (EInitial s'    )
                 Skip s'  → pure $ Skip       (EInitial s'    )
                 Done     → pure $ Done
      go (EPercent s u l) = pure $ Yield u (EUpperHalf s l)
      go (EUpperHalf s l) = pure $ Yield l (EInitial   s  )

decodeStream ∷ ∀f. (Applicative f, Failure DecodeError f)
             ⇒ (Char → Bool)
             → Stream f Word8
             → Stream f DelimitableOctet
{-# INLINE decodeStream #-}
decodeStream isDelim (Stream step (s0 ∷ s) sz)
    = Stream go (DInitial s0) (toMax sz)
    where
      go ∷ DecState s → f (Step (DecState s) DelimitableOctet)
      {-# INLINE go #-}
      go (DInitial s)
          = do r ← step s
               case r of
                 Yield w s'
                     | w ≡ 0x25        → pure $ Skip                (DPercent s')
                     | isDelim (w2c w) → pure $ Yield (Delimiter w) (DInitial s')
                     | otherwise       → pure $ Yield (Literal   w) (DInitial s')
                 Skip    s'            → pure $ Skip                (DInitial s')
                 Done                  → pure Done
      go (DPercent s)
          = do r ← step s
               case r of
                 Yield u s' → pure $ Skip (DUpperHalf s' u)
                 Skip    s' → pure $ Skip (DPercent   s'  )
                 Done       → pure Done
      go (DUpperHalf s u)
          = do r ← step s
               case r of
                 Yield l s' → do w ← decodeHex u l
                                 -- Encoded octets are always literal.
                                 pure $ Yield (Literal w) (DInitial s')
                 Skip    s' → pure    $ Skip (DUpperHalf s' u)
                 Done       → failure $ MissingLowerHalf (w2c u)

encodeHex ∷ Word8 → (Word8, Word8)
{-# INLINEABLE encodeHex #-}
encodeHex w = ( encodeHalf $ w `shiftR` 4
              , encodeHalf $ w .&. 0x0F
              )
    where
      encodeHalf ∷ Word8 → Word8
      {-# INLINEABLE encodeHalf #-}
      encodeHalf h
          | h < 0x0A  = h      + 0x30 -- '0'..'9'
          | otherwise = h - 10 + 0x65 -- 'A'..'F'

decodeHex ∷ (Applicative f, Failure DecodeError f) ⇒ Word8 → Word8 → f Word8
{-# INLINEABLE decodeHex #-}
decodeHex u l
    | (¬) (isHexDigit_w8 u) = failure $ InvalidUpperHalf (w2c u)
    | (¬) (isHexDigit_w8 l) = failure $ InvalidLowerHalf (w2c u) (w2c l)
    | otherwise             = pure    $ unsafeDecodeHex u l

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
