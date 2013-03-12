{-# LANGUAGE
    BangPatterns
  , CPP
  , DeriveDataTypeable
  , FlexibleContexts
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MultiParamTypeClasses
  , OverloadedStrings
  , ScopedTypeVariables
  , TemplateHaskell
  , TypeFamilies
  , UnicodeSyntax
  , ViewPatterns
  #-}
module Data.URI.Internal.Host.IPv6
    ( IPv6Addr(..)
    , ZoneID(..)

    , pIPv6Addr
    , pZoneID

    , bIPv6Addr
    , bZoneID
    )
    where
import Blaze.ByteString.Builder (Builder, Write)
import qualified Blaze.ByteString.Builder as BB
import qualified Blaze.ByteString.Builder.Char8 as BB
import qualified Codec.URI.PercentEncoding as PE
import Control.Applicative
import Control.Applicative.Unicode hiding ((∅))
import Control.DeepSeq
import qualified Data.Attoparsec as B
import Data.Attoparsec.Char8 as C hiding (Done)
import Data.Bits
import Data.CaseInsensitive
import Data.Hashable
import qualified Data.List as L
import Data.Monoid
import Data.Monoid.Unicode
import Data.Semigroup (Semigroup)
import Data.String
import Data.Typeable
import Data.URI.Internal
import Data.URI.Internal.Host.IPv4
import Data.Vector.Fusion.Stream.Monadic
import Data.Vector.Fusion.Stream.Size
import Data.Vector.Generic as GV
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import qualified Data.Vector.Storable.ByteString.Char8 as C8
import Data.Vector.Storable.ByteString.Legacy
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Word (Word8, Word16)
import Numeric.Natural
import Prelude.Unicode
#if defined(MIN_VERSION_QuickCheck)
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
#endif

newtype IPv6Addr = IPv6Addr { unIPv6Addr ∷ UV.Vector Word16 }
    deriving ( Eq
             , Hashable
             , NFData
             , Ord
             , Typeable
             )

newtype ZoneID = ZoneID { unZoneID ∷ ByteString }
    deriving ( Eq
             , FoldCase
             , Hashable
             , Monoid
             , NFData
             , Semigroup
             , Ord
             , Typeable
             )

data CompressedWord16
    = NonZero !Word16 -- ^Literal word
    | Zeroes  !Word16 -- ^The number of consecutive zeroes.

derivingUnbox "CompressedWord16"
    [t| CompressedWord16 → (Bool, Word16) |]
    [| \case
         NonZero w → (True , w)
         Zeroes  n → (False, n)
     |]
    [| \case
         (True , w) → NonZero w
         (False, n) → Zeroes  n
     |]

-- |For testing purpose only.
instance Show ZoneID where
    show = C8.unpack ∘ PE.encode' ((¬) ∘ isUnreserved) ∘ unZoneID

-- |'fromString' constructs a 'ZoneID' from a 'String'. Throws a
-- runtime exception for invalid zone IDs.
instance IsString ZoneID where
    {-# INLINEABLE fromString #-}
    fromString (toLegacyByteString ∘ C8.pack → str)
        = case parseOnly (finishOff pZoneID) str of
            Right s → s
            Left  e → error e

type CompressedIPv6Addr
    = UV.Vector CompressedWord16

data CompState n s
    = Initial          !s
    | CompZero !n      !s
    | LeftOver !Word16 !s
    | Final

compress ∷ IPv6Addr → CompressedIPv6Addr
{-# INLINEABLE compress #-}
compress = GV.unstream ∘ compressStream ∘ GV.stream ∘ unIPv6Addr

compressStream ∷ ∀m. Monad m ⇒ Stream m Word16 → Stream m CompressedWord16
{-# INLINE compressStream #-}
compressStream (Stream step (s0 ∷ s) sz)
    = Stream go (Initial s0) (toMax sz)
    where
      go ∷ CompState Word16 s → m (Step (CompState Word16 s) CompressedWord16)
      {-# INLINEABLE go #-}
      go (Initial s)
          = do r ← step s
               case r of
                 Yield 0 s' → return $ Skip              (CompZero 1 s')
                 Yield w s' → return $ Yield (NonZero w) (Initial    s')
                 Skip    s' → return $ Skip              (Initial    s')
                 Done       → return   Done
      go (CompZero n s)
          = do r ← step s
               case r of
                 Yield 0 s' → return $ Skip              (CompZero (n+1) s')
                 Yield w s' → return $ Yield (Zeroes n)  (LeftOver  w    s')
                 Skip    s' → return $ Skip              (CompZero  n    s')
                 Done       → return $ Yield (Zeroes n)   Final
      go (LeftOver w s)
          = do r ← step s
               case r of
                 Yield 0 s' → return $ Yield (NonZero w) (CompZero 1 s')
                 Yield x s' → return $ Yield (NonZero w) (LeftOver x s')
                 Skip    s' → return $ Yield (NonZero w) (Initial    s')
                 Done       → return $ Yield (NonZero w)  Final
      go Final
          = return Done

pIPv6Addr ∷ Parser IPv6Addr
{-# INLINEABLE pIPv6Addr #-}
pIPv6Addr = IPv6Addr <$>
            choice
            [ --                            6( h16 ":" ) ls32
              do x ← countV 6 (h16 <* char ':')
                 y ← ls32
                 pure $ x ⊕ y

            , --                       "::" 5( h16 ":" ) ls32
              do _ ← string "::"
                 x ← countV 5 (h16 <* char ':')
                 y ← ls32
                 pure $ (∅) `pad` (x ⊕ y)

            , -- [               h16 ] "::" 4( h16 ":" ) ls32
              do x ← option (∅) (GV.singleton <$> h16)
                 _ ← string "::"
                 y ← countV 4 (h16 <* char ':')
                 z ← ls32
                 pure $ x `pad` (y ⊕ z)

            , -- [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
              do x ← option (∅)
                       (GV.cons <$> h16 ⊛ countUpToV 1 (char ':' *> h16))
                 _ ← string "::"
                 y ← countV 3 (h16 <* char ':')
                 z ← ls32
                 pure $ x `pad` (y ⊕ z)

            , -- [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
              do x ← option (∅)
                       (GV.cons <$> h16 ⊛ countUpToV 2 (char ':' *> h16))
                 _ ← string "::"
                 y ← countV 2 (h16 <* char ':')
                 z ← ls32
                 pure $ x `pad` (y ⊕ z)

            , -- [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
              do x ← option (∅)
                       (GV.cons <$> h16 ⊛ countUpToV 3 (char ':' *> h16))
                 _ ← string "::"
                 y ← h16 <* char ':'
                 z ← ls32
                 pure $ x `pad` (y `GV.cons` z)

            , -- [ *4( h16 ":" ) h16 ] "::"              ls32
              do x ← option (∅)
                       (GV.cons <$> h16 ⊛ countUpToV 4 (char ':' *> h16))
                 _ ← string "::"
                 y ← ls32
                 pure $ x `pad` y

            , -- [ *5( h16 ":" ) h16 ] "::"              h16
              do x ← option (∅)
                       (GV.cons <$> h16 ⊛ countUpToV 5 (char ':' *> h16))
                 _ ← string "::"
                 y ← h16
                 pure $ x `pad` GV.singleton y

            , -- [ *6( h16 ":" ) h16 ] "::"
              do x ← option (∅)
                       (GV.cons <$> h16 ⊛ countUpToV 6 (char ':' *> h16))
                 _ ← string "::"
                 pure $ x `pad` (∅)
            ]
            <?>
            "IPv6address"
    where
      h16 ∷ Parser Word16
      {-# INLINEABLE h16 #-}
      h16 = L.foldl' step 0 <$> countUpTo1 4 (B.satisfy isHexDigit_w8)
          where
            step ∷ Word16 → Word8 → Word16
            {-# INLINE step #-}
            step a w = (a `shiftL` 4) .|. htoi w

      ls32 ∷ GV.Vector v Word16 ⇒ Parser (v Word16)
      {-# INLINEABLE ls32 #-}
      ls32 = do x ← h16
                y ← char ':' *> h16
                pure $ GV.fromList [x, y]
             <|>
             (repack <$> pIPv4Addr)
             <?>
             "ls32"

      -- NOTE: This can be implemented using stream operations but I
      -- don't think it's worth it.
      repack ∷ GV.Vector v Word16 ⇒ IPv4Addr → v Word16
      {-# INLINEABLE repack #-}
      repack (IPv4Addr v4)
          = GV.fromList
            [ (fromIntegral (v4 ! 0) `shiftL` 8) .|. fromIntegral (v4 ! 1)
            , (fromIntegral (v4 ! 2) `shiftL` 8) .|. fromIntegral (v4 ! 3)
            ]

      pad ∷ (GV.Vector v Word16, Monoid (v Word16)) ⇒ v Word16 → v Word16 → v Word16
      {-# INLINE pad #-}
      pad x y = let p = GV.replicate (8 - GV.length x - GV.length y) 0
                in
                  x ⊕ p ⊕ y

pZoneID ∷ Parser ZoneID
{-# INLINEABLE pZoneID #-}
pZoneID = do src ← C.takeWhile1 isAllowed
             case PE.decode' (fromLegacyByteString src) of
               Right dst → pure ∘ ZoneID $ dst
               Left  e   → fail $ show (e ∷ PE.DecodeError)
          <?>
          "ZoneID"
    where
      isAllowed ∷ Char → Bool
      {-# INLINE isAllowed #-}
      isAllowed c = isUnreserved c ∨ isPctEncoded c

bIPv6Addr ∷ IPv6Addr → Builder
{-# INLINEABLE bIPv6Addr #-}
bIPv6Addr v6
    -- RFC 5952 recommends IPv4 mixed notation for certain well-known
    -- prefixes.
    | isKnownToBeIPv4Embedded v6 = bIPv4EmbeddedV6Addr v6
    | otherwise                  = bOrdinaryIPv6Addr   v6

isKnownToBeIPv4Embedded ∷ IPv6Addr → Bool
{-# INLINEABLE isKnownToBeIPv4Embedded #-}
isKnownToBeIPv4Embedded (IPv6Addr v6)
    = -- RFC 4291: IPv4-mapped
      prefix96 ≡ GV.fromList [0, 0, 0, 0, 0, 0xFFFF] ∨
      -- RFC 6052: IPv4-translatable
      prefix96 ≡ GV.fromList [0x64, 0xFF9B, 0, 0, 0, 0]
    where
      prefix96 = GV.take 6 v6

bIPv4EmbeddedV6Addr ∷ IPv6Addr → Builder
{-# INLINEABLE bIPv4EmbeddedV6Addr #-}
bIPv4EmbeddedV6Addr (IPv6Addr v6)
    = bOrdinaryIPv6Addr (IPv6Addr $ GV.take 6 v6) ⊕
      BB.fromChar ':'  ⊕
      bIPv4Addr (repack $ GV.drop 6 v6)
    where
      -- NOTE: This can be implemented using stream operations but I
      -- don't think it's worth it.
      repack ∷ GV.Vector v Word16 ⇒ v Word16 → IPv4Addr
      {-# INLINEABLE repack #-}
      repack v = IPv4Addr $
                 GV.fromList
                 [ fromIntegral $ (v ! 0) `shiftR` 8
                 , fromIntegral $ (v ! 0) .&. 0xFF
                 , fromIntegral $ (v ! 1) `shiftR` 8
                 , fromIntegral $ (v ! 1) .&. 0xFF
                 ]

bOrdinaryIPv6Addr ∷ IPv6Addr → Builder
{-# INLINEABLE bOrdinaryIPv6Addr #-}
bOrdinaryIPv6Addr v6 = snd $ GV.ifoldl' go (False, (∅)) cv6
    where
      cv6 = compress v6
      j   = findPosToShorten cv6

      go ∷ (Bool, Builder) → Int → CompressedWord16 → (Bool, Builder)
      {-# INLINEABLE go #-}

      go (sepNeeded, b) _ (NonZero w)
          = if sepNeeded
            then (True, b ⊕ BB.fromChar ':' ⊕ bHex w)
            else (True, b ⊕ bHex w)

      go (sepNeeded, b) i (Zeroes n)
          | j ≡ Just i
              = (False, b ⊕ BB.fromWrite (BB.writeChar ':' ⊕ BB.writeChar ':'))
          | otherwise
              = if sepNeeded
                then (True, b ⊕ BB.fromWrite (BB.writeChar ':' ⊕ wZeroes n))
                else (True, b ⊕ BB.fromWrite (wZeroes n))

wZeroes ∷ ∀n. Whole n ⇒ n → Write
{-# INLINE wZeroes #-}
wZeroes = go (∅)
    where
      go ∷ Write → n → Write
      go !w  0 = w
      go !w  1 = w ⊕ BB.writeChar '0'
      go !w !n = go (BB.writeChar '0' ⊕ BB.writeChar ':' ⊕ w) (n-1)

findPosToShorten ∷ CompressedIPv6Addr → Maybe Int
{-# INLINE findPosToShorten #-}
findPosToShorten = (snd <$>) ∘ GV.ifoldl' go Nothing
    where
      go ∷ Maybe (Word16, Int) → Int → CompressedWord16 → Maybe (Word16, Int)
      {-# INLINEABLE go #-}
      go r             _ (NonZero _) = r
      go Nothing       i (Zeroes  n)
          | n > 1                    = Just (n, i)
          | otherwise                = Nothing
      go (Just (m, j)) i (Zeroes  n)
          | n > m                    = Just (n, i)
          | otherwise                = Just (m, j)

bZoneID ∷ ZoneID → Builder
{-# INLINEABLE bZoneID #-}
bZoneID = BB.fromByteString               ∘
          toLegacyByteString              ∘
          PE.encode' ((¬) ∘ isUnreserved) ∘
          unZoneID

#if defined(MIN_VERSION_QuickCheck)
instance Arbitrary IPv6Addr where
    arbitrary = IPv6Addr ∘ GV.fromList <$> vectorOf 8 arbitrary

instance CoArbitrary IPv6Addr where
    coarbitrary = coarbitrary ∘ GV.toList ∘ unIPv6Addr

instance Arbitrary ZoneID where
    arbitrary = fromString <$> listOf1 (arbitrary `suchThat` isUnreserved)

instance CoArbitrary ZoneID where
    coarbitrary = coarbitrary ∘ C8.unpack ∘ unZoneID
#endif
