{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Binny where

import Data.ByteString qualified as BS
import Data.Word (Word8)
import GHC.Generics (Generic (Rep, from, to), K1 (..), M1 (M1), U1 (U1), (:*:) (..), (:+:) (..))
import Generics.Deriving (V1)

class Binny a where
    bin :: a -> BS.ByteString
    default bin :: (Generic a, Bin' (Rep a)) => a -> BS.ByteString
    bin x = bin' (from x)

    debin :: BS.ByteString -> a
    default debin :: (Generic a, Bin' (Rep a)) => BS.ByteString -> a
    debin = to . debin'

class Bin' f where
    bin' :: f p -> BS.ByteString
    debin' :: BS.ByteString -> f p

instance Bin' V1 where -- Empty
    bin' :: V1 p -> BS.ByteString
    bin' _ = undefined

    debin' :: BS.ByteString -> V1 p
    debin' _ = undefined

instance Bin' U1 where -- Unit
    bin' :: U1 p -> BS.ByteString
    bin' U1 = mempty

    debin' :: BS.ByteString -> U1 p
    debin' _ = U1

instance (Bin' f, Bin' g) => Bin' (f :+: g) where -- Either
    bin' :: (:+:) f g p -> BS.ByteString
    bin' = \case
        L1 l -> BS.pack [0] <> bin' l
        R1 g -> BS.pack [1] <> bin' g

    debin' :: BS.ByteString -> (:+:) f g p
    debin' bs = case BS.unpack bs of
        (0 : es) -> L1 (debin' (BS.pack es))
        (1 : es) -> R1 (debin' (BS.pack es))
        _ -> error "secret third constuctor?"

instance (Bin' f, Bin' g) => Bin' ((:*:) f g) where -- (,)
    bin' :: (:*:) f g p -> BS.ByteString
    bin' (f :*: g) =
        let a = bin' f
            b = bin' g
         in bin (BS.length a) <> a <> b

    debin' :: BS.ByteString -> (:*:) f g p
    debin' bs =
        let aLen = debin bs :: Int
            a = debin' (BS.drop 4 bs)
            b = debin' (BS.drop (aLen + 4) bs)
         in a :*: b

instance (Binny c) => Bin' (K1 i c) where -- a container for a c
    bin' :: K1 i c p -> BS.ByteString
    bin' (K1 x) = bin x

    debin' :: BS.ByteString -> K1 i c p
    debin' bs = K1 (debin bs)

instance (Bin' f) => Bin' (M1 i t f) where -- a wrapper
    bin' :: M1 i t f p -> BS.ByteString
    bin' (M1 m) = bin' m

    debin' :: BS.ByteString -> M1 i t f p
    debin' bs = M1 (debin' bs)

-- | Base implementations
instance Binny Int where
    bin :: Int -> BS.ByteString
    bin 0 = mempty
    bin start = BS.pack . smash $ replicate (8 - length hex) 0 <> hex
      where
        smash [] = []
        smash [_] = error "unaligned"
        smash (a : b : xs) = a * 16 + b : smash xs
        hex = intToHex start

    debin :: BS.ByteString -> Int
    debin bs = case BS.unpack bs of
        (a : b : c : d : _) -> hexToInt . desmash $ [a, b, c, d]
        _ -> error "invalid int data"
      where
        desmash [] = []
        desmash (x : xs) = intToHex x <> desmash xs

intToHex :: (Integral a) => a -> [Word8]
intToHex 0 = []
intToHex i = do
    let dived = i `div` 16
    let moded = i `mod` 16
    intToHex dived <> [fromIntegral moded]

hexToInt :: [Word8] -> Int
hexToInt w = go len w
  where
    go _ [] = 0
    go l (h : hs) = fromIntegral h * 16 ^ l + go (l - 1) hs
    len = length w - 1