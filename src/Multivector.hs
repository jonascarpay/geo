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

data Metric = Positive | Null | Negative

class KnownMetric (s :: Metric) where
  metric :: p s -> Metric

instance KnownMetric Positive where metric _ = Positive

instance KnownMetric Null where metric _ = Null

instance KnownMetric Negative where metric _ = Negative

data Signature = Extend Metric Signature | Empty

data Multivector (sig :: Signature) where
  Scalar :: Double -> Multivector 'Empty
  Dim :: (KnownMetric metric) => Multivector l -> Multivector l -> Multivector (Extend metric l)

instance Num (Multivector 'Empty) where
  Scalar a + Scalar x = Scalar (a + x)
  Scalar a - Scalar x = Scalar (a - x)
  Scalar a * Scalar x = Scalar (a * x)
  negate (Scalar x) = Scalar (negate x)
  fromInteger x = Scalar (fromInteger x)
  abs = error "not implemented"
  signum = error "not implemented"

instance (KnownMetric m, Num (Multivector sig)) => Num (Multivector (Extend m sig)) where
  Dim a b + Dim x y = Dim (a + x) (b + y)

  Dim a b - Dim x y = Dim (a - x) (b - y)

  Dim a b * Dim c d =
    let m = fromInteger (metricD (metric $ Proxy @m))
     in Dim (a * hat d + b * c) (m * a * hat c + b * d)

  negate (Dim a b) = Dim (negate a) (negate b)
  fromInteger x = Dim 0 (fromInteger x)

  abs = error "not implemented"
  signum = error "not implemented"

class Scalar a where
  scalar :: Double -> a

instance Scalar (Multivector 'Empty) where scalar = Scalar

instance
  (Scalar (Multivector sig), KnownMetric m) =>
  Scalar (Multivector (Extend m sig))
  where
  scalar x = Dim (scalar 0) (scalar x)

metricD :: Metric -> Integer
metricD Positive = 1
metricD Null = 0
metricD Negative = -1

class WMetric sig where
  basis :: [Multivector sig]
  zero :: Multivector sig
  one :: Multivector sig

instance WMetric Empty where
  basis = []
  zero = Scalar 0
  one = Scalar 1

instance (KnownMetric m, WMetric sig) => WMetric (Extend m sig) where
  basis = Dim one zero : (Dim zero <$> basis)
  zero = Dim zero zero
  one = Dim zero one

decompose :: Multivector sig -> [(Double, [Int])]
decompose = go 1 []
  where
    go :: Int -> [Int] -> Multivector sig -> [(Double, [Int])]
    go _ ixs (Scalar s) = [(s, ixs)]
    go ix ixs (Dim a b) = go (ix + 1) (ix : ixs) a <> go (ix + 1) ixs b

instance Show (Multivector sig) where
  show mv = intercalate " + " [show a <> " e" <> show b | (a, b) <- decompose mv, a /= 0]

hat :: Multivector sig -> Multivector sig
hat = go 1
  where
    go :: Double -> Multivector sig -> Multivector sig
    go !sign (Scalar k) = Scalar $ sign * k
    go !sign (Dim a b) = Dim (go (-sign) a) (go sign b)
