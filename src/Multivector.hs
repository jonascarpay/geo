{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

module Multivector where

import Data.Data (Proxy (..))
import Data.List (intercalate)
import Signature

data Multivector (sig :: Signature) a where
  Scalar :: a -> Multivector 'Empty a
  Dim :: (KnownMetric metric) => Multivector l a -> Multivector l a -> Multivector (Extend metric l) a

instance Functor (Multivector sig) where
  fmap f (Scalar a) = Scalar (f a)
  fmap f (Dim a b) = Dim (fmap f a) (fmap f b)

instance (Num a) => Num (Multivector 'Empty a) where
  Scalar a + Scalar x = Scalar (a + x)
  Scalar a - Scalar x = Scalar (a - x)
  Scalar a * Scalar x = Scalar (a * x)
  negate (Scalar x) = Scalar (negate x)
  fromInteger x = Scalar (fromInteger x)
  abs = error "not implemented"
  signum = error "not implemented"

instance (Num a, KnownMetric m, Num (Multivector sig a)) => Num (Multivector (Extend m sig) a) where
  Dim a b + Dim x y = Dim (a + x) (b + y)

  Dim a b - Dim x y = Dim (a - x) (b - y)

  Dim a b * Dim c d =
    let m = fromInteger (metricD (metric $ Proxy @m))
     in Dim (a * hat d + b * c) (m * a * hat c + b * d)

  negate (Dim a b) = Dim (negate a) (negate b)
  fromInteger x = Dim 0 (fromInteger x)

  abs = error "not implemented"
  signum = error "not implemented"

metricD :: Metric -> Integer
metricD Positive = 1
metricD Null = 0
metricD Negative = -1

class WMetric sig where
  basis :: (Num a) => [Multivector sig a]
  zero :: (Num a) => Multivector sig a
  one :: (Num a) => Multivector sig a

instance WMetric Empty where
  basis = []
  zero = Scalar 0
  one = Scalar 1

instance (KnownMetric m, WMetric sig) => WMetric (Extend m sig) where
  basis = Dim one zero : (Dim zero <$> basis)
  zero = Dim zero zero
  one = Dim zero one

decompose :: Multivector sig a -> [(a, [Int])]
decompose = go 1 []
  where
    go :: Int -> [Int] -> Multivector sig a -> [(a, [Int])]
    go _ ixs (Scalar s) = [(s, ixs)]
    go ix ixs (Dim a b) = go (ix + 1) (ix : ixs) a <> go (ix + 1) ixs b

instance (Eq a, Num a, Show a) => Show (Multivector sig a) where
  show mv = case [show a <> " e" <> show b | (a, b) <- decompose mv, a /= 0] of
    [] -> "0"
    xs -> intercalate " + " xs

hat :: (Num a) => Multivector sig a -> Multivector sig a
hat = go 1
  where
    go :: (Num a) => a -> Multivector sig a -> Multivector sig a
    go !sign (Scalar k) = Scalar $ sign * k
    go !sign (Dim a b) = Dim (go (-sign) a) (go sign b)
