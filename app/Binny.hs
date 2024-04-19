{-# LANGUAGE DefaultSignatures #-}

module Binny where

import Data.ByteString qualified as BS
import GHC.Generics (Generic (Rep, from), M1, U1 (U1), (:*:), (:+:))
import Generics.Deriving (K1, V1)

class Bin a where
    bin :: a -> BS.ByteString
    default bin :: (Generic a, Bin' (Rep a)) => a -> BS.ByteString
    bin x = bin' (from x)

class Bin' f where
    bin' :: f p -> BS.ByteString

instance Bin' V1 where -- Empty
    bin' :: V1 p -> BS.ByteString
    bin' _ = undefined

instance Bin' U1 where -- Unit
    bin' :: U1 p -> BS.ByteString
    bin' U1 = mempty

instance Bin' ((:+:) f g) where -- Either
    bin' :: (:+:) f g p -> BS.ByteString
    bin' = error ":+:"

instance Bin' ((:*:) f g) where -- (,)
    bin' :: (:*:) f g p -> BS.ByteString
    bin' = error ":*:"

instance Bin' (K1 i c) where -- a container for a c
    bin' :: K1 i c p -> BS.ByteString
    bin' = error "K1"

instance (Show i, Show (t f)) => Bin' (M1 i t f) where -- a wrapper
    bin' :: M1 i t f p -> BS.ByteString
    bin' m = error $ "M1: " <> show m